skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Wallet and UTXO Management lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_wallet_", round(as.numeric(Sys.time())))
  
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
  
  # Prepare addresses
  admin_addr <- mc_get_addresses(conn)[1]
  mc_grant(conn, admin_addr, "receive,send,issue")
  
  # 1. mc_get_wallet_info ------------------------------------------------------
  wallet_info <- mc_get_wallet_info(conn)
  expect_type(wallet_info, "list")
  expect_true("balance" %in% names(wallet_info))
  
  # Create some UTXOs by issuing multiple small amounts
  for(i in 1:3) {
    mi <- mc_issue(conn, admin_addr, paste0("asset_", i), 10)
    mc_wait_for_confirmation(conn, mi)
  }
  
  # 2. mc_list_unspent ---------------------------------------------------------
  unspent <- mc_list_unspent(conn)
  
  expect_s3_class(unspent, "data.frame")
  expect_true(nrow(unspent) >= 3)
  
  # 3. mc_lock_unspent ---------------------------------------------------------
  # Lock the first UTXO
  target_utxo <- list(list(txid = unspent$txid[1], vout = unspent$vout[1]))
  lock_res <- mc_lock_unspent(conn, unlock = FALSE, outputs = target_utxo)
  expect_true(lock_res)
  
  # 4. mc_list_lock_unspent ----------------------------------------------------
  # Verify it appears in the locked list
  locked_list <- mc_list_lock_unspent(conn)
  expect_true(unspent$txid[1] %in% locked_list$txid)
  
  # Unlock it
  unlock_res <- mc_lock_unspent(conn, unlock = TRUE, outputs = target_utxo)
  expect_true(unlock_res)
  expect_equal(nrow(mc_list_lock_unspent(conn)), 0)
  
  # 5. mc_dump_privkey ---------------------------------------------------------
  # Dump private key
  priv_key <- mc_dump_privkey(conn, admin_addr)
  expect_type(priv_key, "character")
  expect_true(nchar(priv_key) > 40)
  
  # Create a new external keypair and import it
  ext_keys <- mc_create_keypairs(conn, count = 1)
  
  # 6. mc_import_privkey -------------------------------------------------------
  import_res <- mc_import_privkey(conn, ext_keys$privkey[1], label = "imported_key", rescan = FALSE)
  expect_null(import_res)
  
  # Verify the address is now in the wallet
  all_addrs <- mc_get_addresses(conn)
  expect_true(ext_keys$address[1] %in% all_addrs)
  
  # 7. mc_combine_unspent ------------------------------------------------------
  # We have multiple small asset UTXOs. Try to combine them.
  # Note: combineunspent might return empty if criteria aren't met, 
  # but the function call should be valid.
  combined_txids <- mc_combine_unspent(conn, mininputs = 2, maxcombines = 1)
  expect_type(combined_txids, "character")
  
  # 8. mc_backup_wallet --------------------------------------------------------
  temp_dir <- tempdir()
  backup_path <- file.path(temp_dir, "wallet_backup.dat")
  dump_path <- file.path(temp_dir, "wallet_dump.txt")
  
  # Backup binary wallet
  expect_null(mc_backup_wallet(conn, backup_path))
  
  # 9. mc_dump_wallet ----------------------------------------------------------
  # Dump text keys
  expect_null(mc_dump_wallet(conn, dump_path))
  
  # Verify files were created (if running on the same machine)
  expect_true(file.exists(backup_path))
  expect_true(file.exists(dump_path))
  
  # 10. mc_import_wallet -------------------------------------------------------
  # Import keys back from the dump file.
  # IMPORTANT: We use rescan = 0 to avoid a full blockchain scan, 
  # which can be very slow and cause the test to timeout.
  expect_null(mc_import_wallet(conn, dump_path, rescan = 0))
  
  
  # Wallet Encryption Lifecycle
  # WARNING: This part of the test is destructive for the connection.
  pass_old <- "strong_password_123"
  pass_new <- "even_stronger_456"
  
  # 11. mc_encrypt_wallet ------------------------------------------------------
  # Encrypt the wallet (Node will stop immediately after success)
  # Some MultiChain versions return a message, others just disconnect.
  try(mc_encrypt_wallet(conn, pass_old), silent = TRUE)
  message("Waiting for node to stop after encryption...")
  Sys.sleep(5)
  
  # Restart node to continue with encrypted wallet
  mc_node_start(chain_name)
  Sys.sleep(5)
  
  # Create new connection after restart
  conn_enc <- mc_connect(config)
  
  # Verify wallet is locked
  w_info_enc <- mc_get_wallet_info(conn_enc)
  expect_false(is.null(w_info_enc$unlocked_until)) # Present if encrypted
  expect_equal(w_info_enc$unlocked_until, 0)       # 0 means locked
  
  # 12. mc_unlock_wallet -------------------------------------------------------
  expect_null(mc_unlock_wallet(conn_enc, pass_old, timeout = 30))
  
  # 13. mc_change_wallet_passphrase ------------------------------------------------
  expect_null(mc_change_wallet_passphrase(conn_enc, pass_old, pass_new))
  
  # 14. mc_lock_wallet ---------------------------------------------------------
  expect_null(mc_lock_wallet(conn_enc))
  
  # Re-verify locked status
  expect_equal(mc_get_wallet_info(conn_enc)$unlocked_until, 0)

})
