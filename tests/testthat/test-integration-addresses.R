skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Full address lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_addr_", round(as.numeric(Sys.time())))
  
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
  
  # 1. mc_get_new_address ------------------------------------------------------
  new_addr <- mc_get_new_address(conn)
  expect_type(new_addr, "character")
  expect_true(nchar(new_addr) > 25)
  
  # 2. mc_validate_address -----------------------------------------------------
  val_info <- mc_validate_address(conn, new_addr)
  expect_true(val_info$isvalid)
  expect_true(val_info$ismine)
  expect_equal(val_info$address, new_addr)
  
  # 3. mc_create_keypairs ------------------------------------------------------
  keypairs <- mc_create_keypairs(conn, count = 2)
  expect_s3_class(keypairs, "data.frame")
  expect_equal(nrow(keypairs), 2)
  expect_true(all(c("address", "pubkey", "privkey") %in% names(keypairs)))
  
  # 4. mc_create_multisig (external) -------------------------------------------
  pkeys <- keypairs$pubkey
  msig_ext <- mc_create_multisig(conn, n_required = 2, keys = pkeys)
  expect_type(msig_ext, "list")
  expect_true("address" %in% names(msig_ext))
  expect_true("redeemScript" %in% names(msig_ext))
  
  # 5. mc_add_multisig_address (into wallet) -----------------------------------
  msig_addr <- mc_add_multisig_address(conn, n_required = 2, keys = pkeys)
  expect_type(msig_addr, "character")
  
  msig_val <- mc_validate_address(conn, msig_addr)
  expect_false(msig_val$ismine)
  expect_true(msig_val$iswatchonly)
  
  # 6. mc_import_address (watch-only) ------------------------------------------
  external_kp <- mc_create_keypairs(conn, count = 1)
  watch_addr <- external_kp$address[1]
  
  mc_import_address(conn, watch_addr, label = "watcher", rescan = FALSE)
  
  watch_val <- mc_validate_address(conn, watch_addr)
  expect_true(watch_val$isvalid)
  expect_true(watch_val$iswatchonly) # Должен быть watch-only
  expect_equal(watch_val$account, "watcher")
  
  # 7. mc_get_addresses --------------------------------------------------------
  all_addrs <- mc_get_addresses(conn)
  expect_true(new_addr %in% all_addrs)
  expect_true(msig_addr %in% all_addrs)
  expect_type(all_addrs, "character")
  
  # 8. mc_list_addresses -------------------------------------------------------
  addr_df <- mc_list_addresses(conn, verbose = TRUE)
  expect_s3_class(addr_df, "data.frame")
  expect_true(nrow(addr_df) >= 2)
  expect_true("address" %in% names(addr_df))
  
  # filter
  subset_df <- mc_list_addresses(conn, addresses = new_addr)
  expect_equal(nrow(subset_df), 1)
  expect_equal(subset_df$address[1], new_addr)
})
