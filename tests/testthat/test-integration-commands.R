skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Node Configuration and Information lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_config_", round(as.numeric(Sys.time())))
  
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
  conn <- mc_connect(config)
  
  # 1. mc_get_info -------------------------------------------------------------
  node_info <- mc_get_info(conn)
  expect_type(node_info, "list")
  expect_true("version" %in% names(node_info))
  expect_true("balance" %in% names(node_info))
  # On a fresh chain, blocks should be 0 or 1 (genesis)
  expect_true(node_info$blocks >= 0)
  
  # 2. mc_get_init_status ------------------------------------------------------
  init_status <- mc_get_init_status(conn)
  expect_type(init_status, "list")
  expect_true("initialized" %in% names(init_status))
  # Since we waited 5s, the node should be initialized
  expect_true(init_status$initialized)
  
  # 3. mc_get_blockchain_params ------------------------------------------------
  bc_params <- mc_get_blockchain_params(conn)
  expect_type(bc_params, "list")
  # Fixed parameters check (e.g., target-block-time is usually 15 by default)
  expect_true("target-block-time" %in% names(bc_params))
  expect_true("protocol-version" %in% names(bc_params))
  
  # 4. mc_get_runtime_params ---------------------------------------------------
  runtime_params <- mc_get_runtime_params(conn)
  expect_type(runtime_params, "list")
  expect_true("miningrequirespeers" %in% names(runtime_params))
  expect_true("maxshowndata" %in% names(runtime_params))
  
  # 5. mc_set_runtime_param ----------------------------------------------------
  # Change miningrequirespeers (logical/boolean)
  res_bool <- mc_set_runtime_param(conn, "miningrequirespeers", FALSE)
  expect_null(res_bool)
  
  # Change maxshowndata (integer)
  # Our function handles the conversion to integer internally
  new_val <- 5001
  res_int <- mc_set_runtime_param(conn, "maxshowndata", new_val)
  expect_null(res_int)
  
  expect_equal(mc_get_runtime_params(conn)$maxshowndata, 5001)
  
  res2 <- mc_set_runtime_param(conn, "acceptfiltertimeout", 42)
  expect_null(res2)
  
  # Verify the changes via getruntimeparams
  updated_params <- mc_get_runtime_params(conn)
  expect_false(updated_params$miningrequirespeers)
  expect_equal(updated_params$maxshowndata, new_val)
  expect_equal(mc_get_runtime_params(conn)$acceptfiltertimeout, 42)
  
  # 6. mc_help -----------------------------------------------------------------
  # General help returns an object of class mc_help
  all_help <- mc_help(conn)
  expect_s3_class(all_help, "mc_help")
  expect_true(grepl("Blockchain", as.character(all_help)))
  
  # Specific command help
  info_help <- mc_help(conn, "getinfo")
  expect_s3_class(info_help, "mc_help")
  expect_true(grepl("getinfo", as.character(info_help)))
  
  # 7. print.mc_help -----------------------------------------------------------
  # Test the S3 print method (output check)
  expect_output(print(info_help), "getinfo")

})
