skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Assets and Tokens lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_assets_", round(as.numeric(Sys.time())))
  
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
  
  # get address
  addr <- mc_get_new_address(conn)
  mc_grant(conn, addr, "receive,issue")
  
  # 1. mc_issue (Fungible Asset) -----------------------------------------------
  # create "gold"
  asset_a <- list(name = "gold", open = TRUE, canclose = TRUE, canopen = TRUE)
  issue_tx <- mc_issue(conn, addr, asset_a, quantity = 1000, units = 0.01)
  expect_type(issue_tx, "character")
  expect_true(nchar(issue_tx) == 64)
  
  # 2. mc_get_asset_info -------------------------------------------------------
  asset_info <- mc_get_asset_info(conn, asset_a$name)
  expect_equal(asset_info$name, asset_a$name)
  expect_equal(asset_info$units, 0.01)
  expect_equal(asset_info$issuetxid, issue_tx)
  
  # 3. mc_issue_more -----------------------------------------------------------
  # issue 500 more
  more_tx <- mc_issue_more(conn, addr, asset_a$name, quantity = 500)
  expect_type(more_tx, "character")
  
  # check update
  updated_info <- mc_get_asset_info(conn, asset_a$name)
  expect_equal(updated_info$issueqty, 1500)
  
  # 4. mc_issue_from -----------------------------------------------------------
  # new asset
  asset_b <- list(name = "silver", open = TRUE, canclose = TRUE, canopen = TRUE)
  issue_from_tx <- mc_issue_from(conn, addr, addr, asset_b, quantity = 100)
  expect_type(issue_from_tx, "character")
  
  asset_b_info <- mc_get_asset_info(conn, asset_b$name)
  expect_equal(asset_b_info$name, asset_b$name)
  
  # 5. mc_issue_more_from -----------------------------------------------------------
  more_from_tx <- mc_issue_more_from(conn, addr, addr, asset_b$name, quantity = 1)
  expect_type(more_from_tx, "character")
  
  # check update
  updated_info <- mc_get_asset_info(conn, asset_b$name)
  expect_equal(updated_info$issueqty, 101)
  
  # 6. mc_update ---------------------------------------------------------------
  # close "silver" for issuing
  update_tx <- mc_update(conn, asset_b$name, list(open = FALSE))
  expect_type(update_tx, "character")
  
  b_info_after <- mc_get_asset_info(conn, asset_b$name)
  expect_false(b_info_after$open)
  
  # 7. mc_update_from ----------------------------------------------------------
  # open "silver" for issuing
  update_from_tx <- mc_update_from(conn, addr, asset_b$name, list(open = TRUE))
  expect_type(update_tx, "character")
  
  b_info_after <- mc_get_asset_info(conn, asset_b$name)
  expect_true(b_info_after$open)
  
  # 8. mc_issue_token ----------------------------------------------------------
  # To issue tokens, parent asset must be created with type = 'nonfungible'
  parent_nft <- list(name = "collection", fungible = FALSE, open = TRUE)
  mc_issue(conn, addr, parent_nft, quantity = 0)
  
  token_name <- "painting1"
  token_tx <- mc_issue_token(conn, addr, parent_nft$name, token_name, 
                             quantity = 1, 
                             token_details = list(artist = "Monet"))
  expect_type(token_tx, "character")
  
  # 9. mc_issue_token_from -----------------------------------------------------
  token_from_tx <- mc_issue_token_from(conn, addr, addr, parent_nft$name, "painting2", 
                                       quantity = 1,
                                       token_details = list(artist = "Sisley"))
  expect_type(token_from_tx, "character")
  
    # 10. mc_get_token_info ------------------------------------------------------
  token_info <- mc_get_token_info(conn, parent_nft$name, token_name, verbose = TRUE)
  expect_equal(token_info$token, token_name)
  expect_equal(token_info$details$artist, "Monet")
  
  token_info <- mc_get_token_info(conn, parent_nft$name, "painting2", verbose = TRUE)
  expect_equal(token_info$token, "painting2")
  expect_equal(token_info$details$artist, "Sisley")
  
  # 11. mc_list_assets ---------------------------------------------------------
  all_assets <- mc_list_assets(conn)
  expect_s3_class(all_assets, "data.frame")
  expect_true(nrow(all_assets) >= 3) # gold, silver, gallery
  expect_true(asset_a$name %in% all_assets$name)
  
  # 12. mc_list_asset_issues ---------------------------------------------------
  issues_df <- mc_list_asset_issues(conn, asset_a$name)
  expect_s3_class(issues_df, "data.frame")
  expect_equal(nrow(issues_df), 2) # primary + mc_issue_more
  
  # 13. mc_list_asset_transactions --------------------------------------------
  # subscribe
  mc_subscribe(conn, asset_a$name)
  
  asset_txs <- mc_list_asset_transactions(conn, asset_a$name, count = 10)
  expect_s3_class(asset_txs, "data.frame")
  expect_true(nrow(asset_txs) >= 2)
  
  # 14. mc_get_asset_transaction -----------------------------------------------
  one_tx_id <- asset_txs$txid[1]
  single_tx <- mc_get_asset_transaction(conn, asset_a$name, one_tx_id)
  expect_type(single_tx, "list")
  expect_equal(single_tx$txid, one_tx_id)

})
