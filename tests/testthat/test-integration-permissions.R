skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Permissions lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_perms_", round(as.numeric(Sys.time())))
  
  # basedir
  if (.Platform$OS.type == "windows") {
    base_dir <- file.path(Sys.getenv("APPDATA"), "MultiChain")
  } else if (Sys.info()["sysname"] == "Darwin") {
    base_dir <- file.path(Sys.getenv("HOME"), "Library/Application Support/MultiChain")
  } else {
    base_dir <- file.path(Sys.getenv("HOME"), ".multichain")
  }
  chain_dir <- file.path(base_dir, chain_name)
  
  # cleanup on exit
  on.exit({
    try(mc_node_stop(chain_name), silent = TRUE)
    Sys.sleep(2)
    if (dir.exists(chain_dir)) unlink(chain_dir, recursive = TRUE)
  })
  
  # init
  mc_node_init(chain_name)
  
  # Speed up block generation (Blockchain Params)
  params_path <- file.path(base_dir, chain_name, "params.dat")
  writeLines(gsub("target-block-time = 15", "target-block-time = 2 ", readLines(params_path)), params_path)
  
  mc_node_start(chain_name)
  
  # wait for node startup
  message("Waiting for node to initialize RPC...")
  Sys.sleep(5) 
  
  config <- mc_get_config(chain_name)
  conn <- mc_connect(config)
  
  # solo mining ON (Runtime Param)
  mc_set_runtime_param(conn, "miningrequirespeers", FALSE)
  
  # The default address created at genesis is our admin
  admin_addr <- mc_get_addresses(conn)[1]
  test_addr_1 <- mc_get_new_address(conn)
  test_addr_2 <- mc_get_new_address(conn)
  
  # 1. mc_grant ----------------------------------------------------------------
  # Grant basic permissions
  g_tx1 <- mc_grant(conn, test_addr_1, "send,receive")
  expect_type(g_tx1, "character")
  
  # wait
  mc_wait_for_confirmation(conn, g_tx1)
  
  # 2. mc_verify_permission ----------------------------------------------------
  expect_true(mc_verify_permission(conn, test_addr_1, "send"))
  expect_true(mc_verify_permission(conn, test_addr_1, "receive"))
  expect_false(mc_verify_permission(conn, test_addr_1, "mine"))
  
  # 3. mc_grant_from -----------------------------------------------------------
  # Grant from the specific admin address
  g_tx2 <- mc_grant_from(conn, admin_addr, test_addr_2, "connect,issue")
  expect_type(g_tx2, "character")
  
  mc_wait_for_confirmation(conn, g_tx2)
  expect_true(mc_verify_permission(conn, test_addr_2, "issue"))
  
  # 4. mc_grant_with_data ------------------------------------------------------
  # Grant with string metadata
  g_tx3 <- mc_grant_with_data(conn, test_addr_1, "mine", data = "Incentivized miner")
  expect_type(g_tx3, "character")
  
  # wait
  mc_wait_for_confirmation(conn, g_tx3)
  
  expect_true(mc_verify_permission(conn, test_addr_1, "mine"))
  
  # 5. mc_grant_with_data_from -------------------------------------------------
  # Grant with JSON metadata from specific admin
  meta <- list(dept = "IT", level = 5)
  g_tx4 <- mc_grant_with_data_from(conn, admin_addr, test_addr_2, "admin", data = meta)
  expect_type(g_tx4, "character")
  
  # wait
  mc_wait_for_confirmation(conn, g_tx4)
  expect_true(mc_verify_permission(conn, test_addr_2, "admin"))
  
  # 6. mc_list_permissions -----------------------------------------------------
  # Check that the permissions appear in the system list
  all_perms <- mc_list_permissions(conn)
  expect_s3_class(all_perms, "data.frame")
  
  # Filter for our test address 1
  addr1_perms <- all_perms[all_perms$address == test_addr_1, ]
  expect_true("send" %in% addr1_perms$type)
  expect_true("mine" %in% addr1_perms$type)
  
  # 7. mc_revoke ---------------------------------------------------------------
  # Remove a permission
  r_tx1 <- mc_revoke(conn, test_addr_1, "mine")
  expect_type(r_tx1, "character")
  
  # wait
  mc_wait_for_confirmation(conn, r_tx1)
  
  expect_false(mc_verify_permission(conn, test_addr_1, "mine"))
  # 'send' should still be there
  expect_true(mc_verify_permission(conn, test_addr_1, "send"))
  
  # 8. mc_revoke_from ----------------------------------------------------------
  # Remove permission from test_addr_2 using the specific admin
  r_tx2 <- mc_revoke_from(conn, admin_addr, test_addr_2, "admin")
  expect_type(r_tx2, "character")
  
  # wait
  mc_wait_for_confirmation(conn, r_tx2)
  expect_false(mc_verify_permission(conn, test_addr_2, "admin"))
  
})