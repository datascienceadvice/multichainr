skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Payments and Transactions lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_payments_", round(as.numeric(Sys.time())))
  
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

  addr_sender <- mc_get_new_address(conn)
  addr_receiver <- mc_get_new_address(conn)
  mc_grant(conn, addr_sender, "send,receive,issue")
  mc_grant(conn, addr_receiver, "send,receive")
  
  # Prepare an asset to send (since native coins are 0 by default)
  asset_name <- "paytoken"
  setup_tx <- mc_issue(conn, addr_sender, asset_name, 1000)
  
  # 1. mc_send -----------------------------------------------------------------
  # Send 10 units of an asset using the general send function
  tx1 <- mc_send(conn, addr_receiver, list(paytoken = 10))
  expect_type(tx1, "character")
  
  # Verify balance
  bal1 <- mc_get_address_balances(conn, addr_receiver)
  expect_true(any(bal1$name == asset_name & bal1$qty >= 10))
  
  # 2. mc_send_asset -----------------------------------------------------------
  # Send 5 units using the single-asset convenience function
  tx2 <- mc_send_asset(conn, addr_receiver, asset_name, 5)
  expect_type(tx2, "character")
  
  # 3. mc_send_asset_from ------------------------------------------------------
  # Send from a specific address to another
  tx3 <- mc_send_asset_from(conn, addr_sender, addr_receiver, asset_name, 5)
  expect_type(tx3, "character")
  
  # 4. mc_send_from ------------------------------------------------------------
  # Send multiple assets (or just one in a list) from a specific address
  tx4 <- mc_send_from(conn, addr_sender, addr_receiver, list(paytoken = 5))
  expect_type(tx4, "character")
  
  # 5. mc_send_with_data -------------------------------------------------------
  # Send with a text note
  test_note <- "Invoice #12345"
  tx5 <- mc_send_with_data(conn, addr_receiver, list(paytoken = 1), test_note)
  expect_type(tx5, "character")
  
  # Verify metadata exists in the transaction
  tx_detail <- mc_get_wallet_transaction(conn, tx5, verbose = TRUE)
  # MultiChain stores metadata in the 'data' field of the output
  expect_true(length(tx_detail$data) > 0)
  
  # Send with structured JSON metadata
  json_meta <- list(id = 500, type = "transfer", internal = TRUE)
  tx6 <- mc_send_with_data(conn, addr_receiver, list(paytoken = 1), json_meta)
  expect_type(tx6, "character")
  
  # 6. mc_send_with_data_from --------------------------------------------------
  # Combine specific sender with metadata
  tx7 <- mc_send_with_data_from(conn, addr_sender, addr_receiver, 
                                list(paytoken = 1), "Final transfer")
  expect_type(tx7, "character")
  
  # Final verification of total units received
  final_bal <- mc_get_address_balances(conn, addr_receiver)
  token_row <- final_bal[final_bal$name == asset_name, ]
  expect_equal(token_row$qty, 21)

})
