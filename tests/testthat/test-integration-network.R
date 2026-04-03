skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Assets and Tokens lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_net_", round(as.numeric(Sys.time())))
  
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
  
  # 1. mc_get_network_info -----------------------------------------------------
  net_info <- mc_get_network_info(conn)
  expect_type(net_info, "list")
  expect_true("version" %in% names(net_info))
  
  # 2. mc_add_node -------------------------------------------------------------
  # 'addnode' manages the connection queue (high-level)
  dummy_node <- "1.1.1.1:8571"
  
  # Add node
  expect_null(mc_add_node(conn, dummy_node, command = "add"))
  
  # Verify in added nodes
  added_nodes <- mc_get_added_node_info(conn, verbose = FALSE)
  expect_true(dummy_node %in% added_nodes)
  
  # Remove node
  expect_null(mc_add_node(conn, dummy_node, command = "remove"))
  
  # 3. mc_store_node (tryconnect) ----------------------------------------------
  # 'storenode' manages the address manager (low-level)
  store_addr <- "2.2.2.2:8571"
  
  # Use 'tryconnect' to add a new node address to the database
  # This avoids Error -701 "Node not found"
  res_store <- mc_store_node(conn, store_addr, command = "tryconnect")
  expect_null(res_store)
  
  # 4. mc_list_stored_nodes ----------------------------------------------------
  stored_df <- mc_list_stored_nodes(conn, include_old_ignores = TRUE)
  expect_s3_class(stored_df, "data.frame")
  
  # Check if our stored node is in the list
  expect_true(store_addr %in% stored_df$addr)
  
  # 5. mc_store_node (ignore) --------------------------------------------------
  # Now that the node exists in the database, we can change its status to 'ignore'
  res_ignore <- mc_store_node(conn, store_addr, command = "ignore")
  expect_null(res_ignore)
  
  # 6. mc_get_peer_info --------------------------------------------------------
  # On a solo node, this should return a data frame (likely empty)
  peers <- mc_get_peer_info(conn)
  expect_s3_class(peers, "data.frame")
  
  # 7. mc_ping -----------------------------------------------------------------
  # Sends a ping to all connected peers
  expect_null(mc_ping(conn))

})
