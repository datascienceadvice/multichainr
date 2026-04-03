skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Blockchain and Mempool information lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_blockchain_", round(as.numeric(Sys.time())))
  
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
  
  # 1. mc_get_blockchain_info --------------------------------------------------
  bc_info <- mc_get_blockchain_info(conn)
  expect_type(bc_info, "list")
  expect_true("blocks" %in% names(bc_info))
  expect_equal(bc_info$chain, "main")
  
  # 2. mc_get_block_hash -------------------------------------------------------
  # Get hash of the genesis block (height 0)
  genesis_hash <- mc_get_block_hash(conn, 0)
  expect_type(genesis_hash, "character")
  expect_equal(nchar(genesis_hash), 64)
  
  # 3. mc_get_block ------------------------------------------------------------
  # Test with height and verbosity 1 (returns list)
  block_0_list <- mc_get_block(conn, 0, verbose = 1)
  expect_type(block_0_list, "list")
  expect_equal(block_0_list$hash, genesis_hash)
  
  # 4. mc_get_mempool_info -----------------------------------------------------
  # Create a transaction to fill the mempool
  addr <- mc_get_new_address(conn)
  mc_grant(conn, addr, "receive,issue")
  
  txid <- mc_issue(conn, addr, "testasset", 1)
  
  mp_info <- mc_get_mempool_info(conn)
  expect_true(mp_info$size >= 1)
  
  # 5. mc_get_raw_mempool ------------------------------------------------------
  raw_mp <- mc_get_raw_mempool(conn)
  expect_true(txid %in% raw_mp)
  
  expect_type(raw_mp, "character")
  expect_true(txid %in% raw_mp)
  
  # 6. mc_get_raw_transaction --------------------------------------------------
  # Get transaction in hex format
  tx_hex <- mc_get_raw_transaction(conn, txid, verbose = FALSE)
  expect_type(tx_hex, "character")
  
  # Get transaction decoded (list)
  tx_list <- mc_get_raw_transaction(conn, txid, verbose = TRUE)
  expect_type(tx_list, "list")
  expect_equal(tx_list$txid, txid)
  
  # 7. mc_get_tx_out -----------------------------------------------------------
  # vout is typically 0 for a simple send
  tx_out <- mc_get_tx_out(conn, txid, 0, unconfirmed = TRUE)
  expect_type(tx_out, "list")
  expect_true("value" %in% names(tx_out))
  
  # 8. mc_get_last_block_info --------------------------------------------------
  last_block <- mc_get_last_block_info(conn)
  expect_type(last_block, "list")
  expect_true(last_block$height >= 1)
  
  # 9. mc_list_blocks ----------------------------------------------------------
  # List the last 2 blocks
  blocks_df <- mc_list_blocks(conn, -2)
  expect_s3_class(blocks_df, "data.frame")
  expect_true(nrow(blocks_df) >= 1)
  
  # 10. mc_get_chain_totals ----------------------------------------------------
  totals <- mc_get_chain_totals(conn)
  expect_type(totals, "list")
  expect_true(totals$blocks >= 1)
  expect_true(totals$assets >= 1)

  # 11. mc_list_miners ---------------------------------------------------------
  miners <- mc_list_miners(conn, verbose = TRUE)
  expect_s3_class(miners, "data.frame")
  # The first node is the miner
  expect_true(nrow(miners) >= 1)
})
