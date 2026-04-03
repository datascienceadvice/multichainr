skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Filters, Libraries, and Variables lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_filters_", round(as.numeric(Sys.time())))
  
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
  
  addr <- mc_get_new_address(conn)
  mc_grant(conn, addr, "admin,create")
  
  # 1. prep_filter_options -----------------------------------------------------
  opts1 <- list(libraries = "mylib")
  res1 <- prep_filter_options(opts1)
  expect_true(is.list(res1$libraries))
  expect_equal(res1$libraries[[1]], "mylib")
  expect_equal(length(res1$libraries), 1)
  
  # 2. mc_create_variable ------------------------------------------------------
  var_name <- "test_var"
  var_tx <- mc_create_variable(conn, var_name, open = TRUE, value = 100)
  expect_type(var_tx, "character")
  
  # 3. mc_create_variable_from -------------------------------------------------
  # Only addresses with 'admin' or 'create' permissions can create variables
  creator_addr <- mc_get_new_address(conn)
  mc_grant(conn, creator_addr, "admin,create")
  
  # --- Execution: Create variable from the specific address ---
  from_var_name <- "addr_linked_var"
  initial_data <- list(origin = "integration_test", priority = 1)
  
  # Use mc_create_variable_from to specify the creator
  create_from_tx <- mc_create_variable_from(
    conn, 
    from_address = creator_addr, 
    name = from_var_name, 
    open = TRUE, 
    value = initial_data
  )
  
  # --- Expectations ---
  expect_type(create_from_tx, "character")
  expect_equal(nchar(create_from_tx), 64)
  
  # Verify the variable metadata and initial value
  # We use verbose = TRUE to ensure we get detailed info if needed
  var_status <- mc_get_variable_info(conn, from_var_name, verbose = TRUE)
  expect_equal(var_status$name, from_var_name)
  
  # Retrieve the actual value stored
  stored_val <- mc_get_variable_value(conn, from_var_name)
  expect_equal(stored_val$origin, "integration_test")
  expect_equal(stored_val$priority, 1)
  
  # Optional: Verify that mc_list_variables sees the new variable
  all_vars_df <- mc_list_variables(conn)
  expect_true(from_var_name %in% all_vars_df$name)
  
  # 4. mc_set_variable_value ---------------------------------------------------
  update_tx <- mc_set_variable_value(conn, var_name, value = list(score = 200))
  expect_type(update_tx, "character")
  
  # 5. mc_set_variable_value_from ----------------------------------------------
  # We use the same address for both creation and update to satisfy ownership rules
  dedicated_addr <- mc_get_new_address(conn)
  mc_grant(conn, dedicated_addr, "admin,create")
  
  # Create the variable FROM the dedicated address
  var_to_update <- "owned_test_var"
  create_tx <- mc_create_variable_from(
    conn, 
    from_address = dedicated_addr, 
    name = var_to_update, 
    open = TRUE, 
    value = list(version = 1.0)
  )
  
  # Execution: Update the variable value FROM the same address
  new_data <- list(version = 2.0, status = "updated", timestamp = as.numeric(Sys.time()))
  
  set_val_from_tx <- mc_set_variable_value_from(
    conn, 
    from_address = dedicated_addr, 
    variable = var_to_update, 
    value = new_data
  )
  
  expect_type(set_val_from_tx, "character")
  expect_equal(nchar(set_val_from_tx), 64)
  
  # Verify the value was actually updated
  updated_val <- mc_get_variable_value(conn, var_to_update)
  expect_equal(updated_val$version, 2.0)
  expect_equal(updated_val$status, "updated")
  
  # Ensure the update transaction is visible in history
  var_history <- mc_get_variable_history(conn, var_to_update, verbose = TRUE)
  expect_s3_class(var_history, "data.frame")
  expect_true(nrow(var_history) >= 2)
  # Verify the txid of the update matches the one we received
  expect_true(set_val_from_tx %in% var_history$txid)
  
  # 6. mc_get_variable_value ---------------------------------------------------
  current_val <- mc_get_variable_value(conn, var_name)
  expect_equal(current_val$score, 200)
  
  # 7. mc_get_variable_info ----------------------------------------------------
  var_info <- mc_get_variable_info(conn, var_name)
  expect_equal(var_info$name, var_name)
  
  # 8. mc_get_variable_history -------------------------------------------------
  history <- mc_get_variable_history(conn, var_name, count = 10)
  expect_s3_class(history, "data.frame")
  expect_true(nrow(history) >= 2) # Create + Update
  
  # 9. mc_list_variables -------------------------------------------------------
  all_vars <- mc_list_variables(conn)
  expect_true(var_name %in% all_vars$name)
  
  # 10. mc_create_library ------------------------------------------------------
  lib_name <- "math_lib"
  js_code <- "function add(a, b) { return a + b; }"
  lib_tx <- mc_create_library(conn, lib_name, updatemode = "instant", js_code = js_code)
  expect_type(lib_tx, "character")
  
  # 11. mc_add_library_update --------------------------------------------------
  new_js <- "function add(a, b) { return a + b; } function sub(a, b) { return a - b; }"
  lib_up_tx <- mc_add_library_update(conn, lib_name, "v2", new_js)
  expect_type(lib_up_tx, "character")
  
  # 12. mc_add_library_update_from ---------------------------------------------
  test_addr <- mc_get_new_address(conn)
  mc_grant(conn, test_addr, "admin,create")
  
  # Create the library explicitly FROM this address
  lib_name <- "verified_lib"
  js_v1 <- "function info() { return 1; }"
  
  # Since your mc_create_library doesn't support 'from_address', 
  # we use the raw mc_rpc to ensure this address becomes the 'owner'
  lib_create_tx <- multichainr:::mc_rpc(conn, "createfrom", list(
    test_addr, 
    "library", 
    lib_name, 
    list(updatemode = "instant"), 
    js_v1
  ))
  
  # Add library update from the SAME address
  update_name <- "version_2"
  js_v2 <- "function info() { return 2; }"
  
  # This will now succeed because test_addr is the proven owner
  lib_update_tx <- mc_add_library_update_from(conn, test_addr, lib_name, update_name, js_v2)
  
  expect_type(lib_update_tx, "character")
  expect_equal(nchar(lib_update_tx), 64)
  
  # Final verification
  updated_code <- mc_get_library_code(conn, lib_name, updatename = update_name)
  expect_equal(updated_code, js_v2)
  
  # 13. mc_get_library_code ----------------------------------------------------
  lib_data <- mc_get_library_code(conn, lib_name)
  expect_equal(lib_data, js_v2)
  
  # 14. mc_list_libraries ------------------------------------------------------
  libs_df <- mc_list_libraries(conn)
  expect_true(lib_name %in% libs_df$name)
  
  # 15. mc_test_library --------------------------------------------------------
  test_res <- mc_test_library(conn, js_code = "function test() { return 1; }")
  expect_type(test_res, "list")
  
  # 16. mc_create_stream_filter ------------------------------------------------
  # Note: Filters often require library dependencies or specific logic
  filter_name <- "simple_filter"
  filter_js <- "function filterstreamitem() { return true; }"
  
  # Use prep_filter_options logic via mc_create_stream_filter
  sf_tx <- mc_create_stream_filter(conn, filter_name, list(libraries = lib_name), filter_js)
  expect_type(sf_tx, "character")
  
  # 17. mc_list_stream_filters -------------------------------------------------
  s_filters <- mc_list_stream_filters(conn)
  expect_true(filter_name %in% s_filters$name)
  
  # 18. mc_run_stream_filter ---------------------------------------------------
  # Setup a stream and item to test against
  stream_name <- "f_stream"
  mc_create_stream(conn, stream_name)
  item_tx <- mc_publish(conn, stream_name, "key", "00")
  
  # Run existing filter
  run_res <- mc_run_stream_filter(conn, filter_name, item_tx)
  expect_type(run_res, "list")
  
  # 19. mc_test_stream_filter --------------------------------------------------
  # Test new filter code
  test_f_res <- mc_test_stream_filter(conn, FALSE, "function filterstreamitem() { return true; }", item_tx)
  expect_type(test_f_res, "list")
  expect_true(test_f_res$compiled)
  
  # 20. mc_test_tx_filter ------------------------------------------------------
  asset_name <- "filtered_coin"
  asset_tx <- mc_issue(conn, addr, asset_name, 1000)
  filter_js <- "function filtertransaction() {
      var tx = getfiltertransaction();
      for (var i = 0; i < tx.vout.length; i++) {
          if (tx.vout[i].assets) {
              for (var j = 0; j < tx.vout[i].assets.length; j++) {
                  if (tx.vout[i].assets[j].name == 'filtered_coin' && tx.vout[i].assets[j].qty > 50) {
                      return 'Amount exceeds limit of 50';
                  }
              }
          }
      }
      return null;
  }"
  
  # Test the filter logic before permanently creating it on the blockchain.
  # We use the asset_name in the 'for' restriction.
  test_res <- mc_test_tx_filter(conn, list("for" = asset_name), filter_js)
  expect_type(test_res, "list")
  expect_true(test_res$compiled)
  
  # 21. mc_create_tx_filter ----------------------------------------------------
  filter_name <- "limit_qty_filter"
  create_tx <- mc_create_tx_filter(conn, filter_name, list("for" = asset_name), filter_js)
  expect_type(create_tx, "character")
  
  # 22. mc_list_tx_filters -----------------------------------------------------
  all_filters <- mc_list_tx_filters(conn)
  expect_s3_class(all_filters, "data.frame")
  expect_true(filter_name %in% all_filters$name)
  
  # 23. mc_run_tx_filter -------------------------------------------------------
  # We run the existing filter against the asset issuance transaction.
  # Since the issuance was 1000 units (> 50), the filter should return the error.
  run_res <- mc_run_tx_filter(conn, filter_name, asset_tx)
  expect_type(run_res, "list")
  
  # Check if the filter logic was triggered (it should return our error string)
  # MultiChain returns the result in the 'filterresult' field
  expect_equal(run_res$reason, "Amount exceeds limit of 50")
  
  # 24. mc_get_filter_code -----------------------------------------------------
  f_code <- mc_get_filter_code(conn, filter_name)
  expect_equal(f_code, filter_js)
  
  # 25. mc_create_upgrade ------------------------------------------------------
  upgrade_name <- "test_upgrade"
  up_tx <- mc_create_upgrade(conn, upgrade_name, list("target-block-time" = 10))
  expect_type(up_tx, "character")
  
  # 26. mc_list_upgrades -------------------------------------------------------
  upgrades <- mc_list_upgrades(conn)
  expect_true(upgrade_name %in% upgrades$name)
  
  # 27. mc_approve_from --------------------------------------------------------
  # Approve the upgrade we just created
  app_tx <- mc_approve_from(conn, addr, upgrade_name, approve = TRUE)
  expect_type(app_tx, "character")
  
})
