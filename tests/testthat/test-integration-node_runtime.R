skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Node Runtime control lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_runtime_", round(as.numeric(Sys.time())))
  
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
  
  # 1-2. mc_pause / mc_resume ----------------------------------------------------
  # Test pausing and resuming specific tasks
  expect_equal(mc_pause(conn, "offchain"), "Paused")
  expect_equal(mc_resume(conn, "offchain"), "Resumed")
  
  # Test multiple tasks as a vector
  expect_equal(mc_pause(conn, c("incoming", "mining")), "Paused")
  expect_equal(mc_resume(conn, c("incoming", "mining")), "Resumed")
  
  # 3. mc_clear_mempool --------------------------------------------------------
  # Create a transaction to fill the mempool
  addr <- mc_get_new_address(conn)
  mc_grant(conn, addr, "receive")
  
  # Issuing an asset puts a transaction in the mempool
  mc_issue(conn, addr, "mempooltest", 1)
  
  # Verify mempool is not empty
  expect_true(mc_get_mempool_info(conn)$size > 0)
  
  # MultiChain requires node to be paused for clearing mempool
  mc_pause(conn, "incoming,mining")
  
  # Clear the mempool
  expect_equal(mc_clear_mempool(conn), "Mempool cleared")
  
  # Verify mempool is now empty
  expect_equal(mc_get_mempool_info(conn)$size, 0)
  
  # Resume node
  mc_resume(conn, "incoming,mining")
  
  # 4. mc_get_chunk_queue_info -------------------------------------------------
  # Off-chain data statistics
  queue_info <- mc_get_chunk_queue_info(conn)
  expect_type(queue_info, "list")
  
  # 5. mc_get_chunk_queue_totals -----------------------------------------------
  queue_totals <- mc_get_chunk_queue_totals(conn)
  expect_type(queue_totals, "list")
  
  # 6. mc_set_last_block (Rewind) ----------------------------------------------
  # First, ensure we have at least one block beyond genesis
  Sys.sleep(2)
  mc_grant(conn, addr, "receive")
  txid <- mc_issue(conn, addr, "rewindtest", 1)
  
  # Current height should be at least 1
  initial_info <- mc_get_blockchain_info(conn)
  initial_height <- initial_info$blocks
  expect_true(initial_height >= 1)
  
  # MultiChain requires a pause to rewind
  mc_pause(conn, "incoming,mining")
  
  # Rewind to block 0 (Genesis)
  genesis_hash <- mc_get_block_hash(conn, 0)
  
  # The function returns the hash of the new tip
  new_tip_hash <- mc_set_last_block(conn, genesis_hash)
  expect_type(new_tip_hash, "character")
  expect_equal(new_tip_hash, genesis_hash)
  
  
  # Verify current height is back to 0
  post_rewind_info <- mc_get_blockchain_info(conn)
  expect_equal(post_rewind_info$blocks, 0)

  # Resume
  mc_resume(conn, "incoming,mining")
  
})
