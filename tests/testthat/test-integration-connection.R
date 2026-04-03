skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Connection management and Authentication", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_conn_", round(as.numeric(Sys.time())))
  
  # basedir
  if (.Platform$OS.type == "windows") {
    base_dir <- file.path(Sys.getenv("APPDATA"), "MultiChain")
  } else if (Sys.info()["sysname"] == "Darwin") {
    base_dir <- file.path(Sys.getenv("HOME"), "Library/Application Support/MultiChain")
  } else {
    base_dir <- file.path(Sys.getenv("HOME"), ".multichain")
  }
  chain_dir <- file.path(base_dir, chain_name)
  
  # clenaup on exit
  on.exit({
    try(mc_node_stop(chain_name), silent = TRUE)
    Sys.sleep(2)
    if (dir.exists(chain_dir)) unlink(chain_dir, recursive = TRUE)
  })
  
  # init
  mc_node_init(chain_name)
  mc_node_start(chain_name)
  
  # wait
  message("Waiting for node to initialize RPC...")
  Sys.sleep(5) 
  
  config <- mc_get_config(chain_name)
  
  # 1. mc_connect --------------------------------------------------------------
  # Config load
  conn_list <- mc_connect(config)
  
  expect_s3_class(conn_list, "multichain_conn")
  expect_equal(conn_list$user, config$user)
  expect_true(grepl(as.character(config$port), conn_list$url))
  
  # Testing manual construction of the connection object -----------------------
    conn_explicit <- mc_connect(
    host     = "127.0.0.1", 
    port     = config$port, 
    user     = config$user, 
    password = config$password
  )
  
  expect_s3_class(conn_explicit, "multichain_conn")
  expect_equal(conn_explicit$password, config$password)
  
  # Verify Authentication (Integration Step) -----------------------------------
  # Ensure the constructed connection actually works against the node
  node_info <- mc_get_info(conn_list)
  expect_type(node_info, "list")
  expect_equal(node_info$chainname, chain_name)
  
  # 2. print.multichain_conn ---------------------------------------------------
  # Verify that the S3 print method hides the sensitive password
  output <- capture.output(print(conn_list))
  
  expect_true(any(grepl("URL:", output)))
  expect_true(any(grepl("User:", output)))
  expect_true(any(grepl("\\[HIDDEN\\]", output)))
  
  # Ensure the actual password string is NOT in the printed output
  expect_false(any(grepl(config$password, output)))
  
  # Connection Failure ---------------------------------------------------------
  # Verify that a connection with wrong credentials fails during an RPC call
  bad_conn <- mc_connect(
    host     = "127.0.0.1", 
    port     = config$port, 
    user     = config$user, 
    password = "wrong_password"
  )
  
  expect_error(mc_get_info(bad_conn), "401 Unauthorized")
  
})
