skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Raw Transactions full lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_raw_", round(as.numeric(Sys.time())))
  
  if (.Platform$OS.type == "windows") {
    base_dir <- file.path(Sys.getenv("APPDATA"), "MultiChain")
  } else if (Sys.info()["sysname"] == "Darwin") {
    base_dir <- file.path(Sys.getenv("HOME"), "Library/Application Support/MultiChain")
  } else {
    base_dir <- file.path(Sys.getenv("HOME"), ".multichain")
  }
  chain_dir <- file.path(base_dir, chain_name)
  
  on.exit({
    try(mc_node_stop(chain_name), silent = TRUE)
    Sys.sleep(2)
    if (dir.exists(chain_dir)) unlink(chain_dir, recursive = TRUE)
  })
  
  mc_node_init(chain_name)
  
  # Set fast block time
  params_path <- file.path(base_dir, chain_name, "params.dat")
  writeLines(gsub("target-block-time = 15", "target-block-time = 2 ", readLines(params_path)), params_path)
  
  mc_node_start(chain_name)
  Sys.sleep(5) 
  
  config <- mc_get_config(chain_name)
  conn <- mc_connect(config)
  
  addr_admin <- mc_get_addresses(conn)[1]
  addr_recipient <- mc_get_new_address(conn)
  mc_grant(conn, addr_recipient, "receive")
  
  asset_name <- "rawtoken"
  # Issue 100 units (this creates one UTXO with 100 tokens)
  issue_tx <- mc_issue(conn, addr_admin, asset_name, 100)
  mc_wait_for_confirmation(conn, issue_tx)
  
  # 1. mc_create_raw_send_from -------------------------------------------------
  # Admin sends 10 tokens to recipient. 
  # This spends the 100-token UTXO and creates a new 90-token UTXO for admin.
  to_amounts_1 <- list()
  to_amounts_1[[addr_recipient]] <- list(rawtoken = 10)
  
  raw_send_res <- mc_create_raw_send_from(conn, addr_admin, to_amounts_1, action = "send")
  mc_wait_for_confirmation(conn, raw_send_res)
  
  # FIND THE CORRECT UTXO
  # We need a UTXO belonging to admin that has at least 6 tokens (5 + 1 for next steps)
  unspent <- mc_list_unspent(conn, addresses = addr_admin)
  
  # Look through the nested list of assets in each UTXO row
  target_row_idx <- which(sapply(unspent$assets, function(asset_list) {
    if (length(asset_list) == 0) return(FALSE)
    # Check if this UTXO contains our asset with quantity >= 6
    any(sapply(asset_list, function(a) a$name == asset_name && a$qty >= 6))
  }))[1]
  
  skip_if(is.na(target_row_idx), "Could not find a UTXO with enough tokens for manual test.")
  
  utxo <- unspent[target_row_idx, ]
  inputs <- list(list(txid = utxo$txid, vout = utxo$vout))
  
  # 2. mc_create_raw_transaction -----------------------------------------------
  # Build initial hex (5 tokens)
  outputs_manual <- list()
  outputs_manual[[addr_recipient]] <- list(rawtoken = 5)
  tx_hex <- mc_create_raw_transaction(conn, inputs, outputs_manual)
  
  # 3. mc_append_raw_data ------------------------------------------------------
  # Append Data
  tx_hex <- mc_append_raw_data(conn, tx_hex, list(info = "step-by-step"))
  
  # 4. mc_append_raw_transaction -----------------------------------------------
  # Append Additional Output (1 token)
  # IMPORTANT: Do this BEFORE adding change
  addr_extra <- mc_get_new_address(conn)
  mc_grant(conn, addr_extra, "receive")
  
  outputs_extra <- list()
  outputs_extra[[addr_extra]] <- list(rawtoken = 1)
  tx_hex <- mc_append_raw_transaction(conn, tx_hex, outputs = outputs_extra)
  
  # 5. mc_append_raw_change ----------------------------------------------------
  # Append Change
  # Now Inputs (e.g. 90) > Outputs (5 + 1). This should now succeed.
  tx_hex <- mc_append_raw_change(conn, tx_hex, addr_admin)
  expect_type(tx_hex, "character")
  
  # 6. mc_decode_raw_transaction -----------------------------------------------
  # Decode
  decoded <- mc_decode_raw_transaction(conn, tx_hex)
  expect_true(length(decoded$vout) >= 4)
  
  # 7. mc_sign_raw_transaction -------------------------------------------------
  # prepare parents_data for sign
  # extract data from AsIs
  # scriptPubKey[[1]] - string
  # assets[[1]] - list of objects
  parents_data <- list(list(
    txid         = utxo$txid,
    vout         = as.integer(utxo$vout),
    scriptPubKey = utxo$scriptPubKey[[1]], 
    amount       = as.numeric(utxo$amount),
    assets       = utxo$assets[[1]]
  ))
  
  # private_keys = NULL (default), to sign with own wallet
  signed_res <- mc_sign_raw_transaction(conn, tx_hex, parents = parents_data)
  
  expect_true(signed_res$complete)

  # 8. mc_send_raw_transaction -------------------------------------------------
  final_txid <- mc_send_raw_transaction(conn, signed_res$hex)
  mc_wait_for_confirmation(conn, final_txid)
  expect_type(final_txid, "character")
  
  # Verify final balance
  mc_wait_for_confirmation(conn, final_txid)
  balances <- mc_get_address_balances(conn, addr_recipient)
  # Total received by recipient: 10 (automated) + 5 (manual) = 15
  expect_equal(balances[balances$name == asset_name, ]$qty, 15)
  
  extra_bal <- mc_get_address_balances(conn, addr_extra)
  expect_equal(extra_bal[extra_bal$name == asset_name, ]$qty, 1)
})