skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Subscriptions (Streams and Assets) lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_sub_", round(as.numeric(Sys.time())))
  
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
  
  # Address setup: Subscription requires 'receive' permissions on the node
  admin_addr <- mc_get_addresses(conn)[1]
  mc_grant(conn, admin_addr, "receive")
  
  # Create test entities
  stream_name <- "sub_test_stream"
  asset_name <- "sub_test_asset"
  
  tx_s <- mc_create_stream(conn, stream_name)
  tx_a <- mc_issue(conn, admin_addr, asset_name, 1000)
  
  mc_wait_for_confirmation(conn, tx_s)
  mc_wait_for_confirmation(conn, tx_a)
  
  # 1. mc_subscribe ------------------------------------------------------------
  res_sub_s <- mc_subscribe(conn, stream_name, rescan = TRUE)
  expect_null(res_sub_s)
  expect_true(mc_list_streams(conn, stream_name)$subscribed[1])
  
  # multisubscription
  entities <- c(stream_name, asset_name)
  res_multi <- mc_subscribe(conn, entities)
  expect_null(res_multi)
  
  expect_true(mc_list_streams(conn, stream_name)$subscribed[1])
  expect_true(mc_list_assets(conn, asset_name)$subscribed[1])

  
  # 2. mc_unsubscribe ----------------------------------------------------------
  res_unsub_s <- mc_unsubscribe(conn, stream_name)
  expect_null(res_unsub_s)
  
  res_unsub_a <- mc_unsubscribe(conn, asset_name)
  expect_null(res_unsub_a)
  
  # multi unsubscribe
  expect_false(mc_list_streams(conn, stream_name)$subscribed[1])
  expect_false(mc_list_assets(conn, asset_name)$subscribed[1])

  # The purge flag instructs the node to delete locally stored metadata/offchain data
  res_purge <- mc_unsubscribe(conn, stream_name, purge = TRUE)
  expect_null(res_purge)
  
  expect_false(mc_list_streams(conn, stream_name)$subscribed[1])
})
