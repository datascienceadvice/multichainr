skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Atomic Exchange (Trade) workflow", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_exchange_", round(as.numeric(Sys.time())))
  
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
  
  addr1 <- mc_get_new_address(conn)
  addr2 <- mc_get_new_address(conn)
  mc_grant(conn, addr1, "receive,send")
  mc_grant(conn, addr2, "receive,send")
  
  # Prepare assets for exchange
  asset_a <- "tokenA"
  asset_b <- "tokenB"
  tx_a <- mc_issue(conn, addr1, asset_a, 100)
  tx_b <- mc_issue(conn, addr2, asset_b, 100)

  # 1. mc_prepare_lock_unspent -------------------------------------------------
  # Party 1 prepares 10 units of tokenA for the trade
  lock1 <- mc_prepare_lock_unspent(conn, amounts = list(tokenA = 10))
  expect_type(lock1$txid, "character")
  expect_type(lock1$vout, "integer")
  
  # 2. mc_prepare_lock_unspent_from --------------------------------------------
  # Party 2 prepares 5 units of tokenB from a specific address
  lock2 <- mc_prepare_lock_unspent_from(conn, addr2, amounts = list(tokenB = 5))
  expect_type(lock2$txid, "character")
  
  # 3. mc_create_raw_exchange --------------------------------------------------
  # Party 1 creates an offer: 10 tokenA in exchange for 5 tokenB
  offer_hex <- mc_create_raw_exchange(conn, lock1$txid, lock1$vout, 
                                      amounts = list(tokenB = 5))
  expect_type(offer_hex, "character")
  
  # 4. mc_decode_raw_exchange --------------------------------------------------
  # Inspect the offer to ensure it matches expectations
  decoded <- mc_decode_raw_exchange(conn, offer_hex, verbose = TRUE)
  expect_type(decoded, "list")
  # MultiChain trade decoding structure check
  expect_true("offer" %in% names(decoded))
  expect_equal(decoded$offer$assets[[1]]$name, "tokenA")
  
  # 5. mc_append_raw_exchange --------------------------------------------------
  # In a multi-party scenario, we could append more stages. 
  # Here we verify the function accepts the parameters.
  appended <- mc_append_raw_exchange(conn, offer_hex, lock2$txid, lock2$vout, 
                                     amounts = list(tokenA = 10))
  expect_type(appended$hex, "character")
  expect_type(appended$complete, "logical")
  
  # 6. mc_complete_raw_exchange ------------------------------------------------
  # Party 2 accepts and finalizes the exchange
  final_hex <- mc_complete_raw_exchange(conn, offer_hex, lock2$txid, lock2$vout, 
                                        amounts = list(tokenA = 10),
                                        data = "trade complete")
  expect_type(final_hex, "character")
  
  # Verify the final hex can be sent (broadcast test)
  # We won't actually send it to avoid complicating the wallet state, 
  # but we check that the hex was produced.
  expect_true(nchar(final_hex) > nchar(offer_hex))
  
  # 7. mc_disable_raw_transaction ----------------------------------------------
  # Invalidate the offer by spending the locked outputs back to the owner
  # This creates a transaction that makes the offer_hex unusable
  disable_txid <- mc_disable_raw_transaction(conn, offer_hex)
  expect_type(disable_txid, "character")
  expect_equal(nchar(disable_txid), 64)

})
