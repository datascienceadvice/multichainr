skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Balances and Transactions lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_trans_", round(as.numeric(Sys.time())))
  
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
  
  # Address Preparation
  addr_admin <- mc_get_addresses(conn)[1]
  addr_user <- mc_get_new_address(conn)
  mc_grant(conn, addr_user, "receive,send")
  
  # Asset Preparation
  asset_f <- "coin"
  asset_nft <- "collection"
  
  # Issue fungible asset
  tx_f <- mc_issue(conn, addr_admin, asset_f, 1000, units = 0.01)
  # Issue parent for NFTs
  tx_p <- mc_issue(conn, addr_admin, list(name = asset_nft, fungible = FALSE, open = TRUE), quantity = 0)
  # Issue specific NFT token
  tx_token <- mc_issue_token(conn, addr_admin, asset_nft, "token1", 1)
  
  mc_wait_for_confirmation(conn, tx_token)
  
  # 1. mc_get_total_balances ---------------------------------------------------
  # Check total balances
  total_bal <- mc_get_total_balances(conn)
  expect_s3_class(total_bal, "data.frame")
  expect_true(asset_f %in% total_bal$name)
  
  # 2. mc_get_address_balances -------------------------------------------------
  # Check specific address balances
  admin_bal <- mc_get_address_balances(conn, addr_admin)
  expect_equal(admin_bal$qty[admin_bal$name == asset_f], 1000)
  
  # Send assets from admin to user
  send_txid <- mc_send_asset(conn, addr_user, asset_f, 100)
  
  # Test the blocking wait function
  mc_wait_for_confirmation(conn, send_txid)
  
  # Verify user received the assets
  user_bal <- mc_get_address_balances(conn, addr_user)
  expect_equal(user_bal$qty[user_bal$name == asset_f], 100)
  
  # 3. mc_get_multi_balances ---------------------------------------------------
  # Get balances for multiple addresses at once
  multi_bal <- mc_get_multi_balances(conn, addresses = c(addr_admin, addr_user))
  
  expect_type(multi_bal, "list")
  expect_true(addr_admin %in% names(multi_bal))
  expect_true(addr_user %in% names(multi_bal))
  
  admin_assets <- sapply(multi_bal[[addr_admin]], function(x) x$name)
  expect_true(asset_f %in% admin_assets)
  
  # Get NFT balances
  token_bal <- mc_get_token_balances(conn, addresses = addr_admin, assets = asset_nft)
  expect_s3_class(token_bal, "data.frame")
  expect_true("token" %in% names(token_bal))
  expect_equal(token_bal$token[1], "token1")
  
  # 4. mc_get_wallet_transaction -----------------------------------------------
  # Get wallet transaction details
  tx_detail <- mc_get_wallet_transaction(conn, send_txid, verbose = TRUE)
  expect_type(tx_detail, "list")
  expect_equal(tx_detail$txid, send_txid)
  expect_true("vin" %in% names(tx_detail))
  
  # 5. mc_get_address_transaction ----------------------------------------------
  # Get transaction details specific to an address
  addr_tx_detail <- mc_get_address_transaction(conn, addr_user, send_txid)
  expect_equal(addr_tx_detail$txid, send_txid)
  
  # 6. mc_list_wallet_transactions ---------------------------------------------
  # List history for the whole wallet
  wallet_history <- mc_list_wallet_transactions(conn, count = 5)
  expect_s3_class(wallet_history, "data.frame")
  expect_true(send_txid %in% wallet_history$txid)
  
  # 7. mc_list_address_transactions --------------------------------------------
  user_history <- mc_list_address_transactions(conn, addr_user, count = 10)
  expect_s3_class(user_history, "data.frame")
  expect_true(send_txid %in% user_history$txid)
  
  # 8. mc_get_wallet_transaction -----------------------------------------------
  # Create a transaction with a larger data payload
  large_text <- paste(rep("MultiChain data integration test. ", 10), collapse = "")
  data_txid <- mc_send_with_data(conn, addr_user, list(coin = 1), large_text)
  mc_wait_for_confirmation(conn, data_txid)
  
  # Standard tx queries often truncate data. Use gettxoutdata to get the full hex.
  # The data is usually in vout 0 or 1 depending on the command structure.
  # We find the vout with the data first.
  tx_info <- mc_get_wallet_transaction(conn, data_txid, verbose = TRUE)
  
  # 9. mc_get_tx_out_data ------------------------------------------------------
  # Identify the data output index
  vout_index <- -1
  for(i in seq_along(tx_info$vout)) {
    if(!is.null(tx_info$vout[[i]]$data)) {
      vout_index <- tx_info$vout[[i]]$n
      break
    }
  }
  
  skip_if(vout_index == -1, "Could not find data output in transaction")
  
  full_hex <- mc_get_tx_out_data(conn, data_txid, vout_index)
  expect_type(full_hex, "character")
  
  # Decode and verify
  decoded_text <- hex_to_char(full_hex)
  expect_true(grepl("MultiChain data", decoded_text))
  
  # Test partial retrieval
  partial_hex <- mc_get_tx_out_data(conn, data_txid, vout_index, count_bytes = 10, start_byte = 0)
  expect_equal(nchar(partial_hex), 20) # 10 bytes = 20 hex chars
})
