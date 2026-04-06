skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Streams and Stream Items full lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_streams_", round(as.numeric(Sys.time())))
  
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
  user_addr <- mc_get_new_address(conn)
  mc_grant(conn, user_addr, "receive,send,create") # Needs 'create' to make streams
  
  # 1. mc_create_stream --------------------------------------------------------
  stream_open <- "public_stream"
  stream_restricted <- "private_stream"
  
  # Create an open stream
  tx_create_1 <- mc_create_stream(conn, stream_open, open = TRUE)
  expect_type(tx_create_1, "character")
  
  # 2. mc_create_stream_from ---------------------------------------------------
  # Create a restricted stream from a specific address with custom fields
  custom_meta <- list(project = "research", version = "1.0")
  tx_create_2 <- mc_create_stream_from(conn, user_addr, stream_restricted, 
                                       open = list(restrict = "write"),
                                       custom_fields = custom_meta)
  expect_type(tx_create_2, "character")
  
  mc_wait_for_confirmation(conn, tx_create_1)
  mc_wait_for_confirmation(conn, tx_create_2)
  
  # 3. mc_get_stream_info ------------------------------------------------------
  # Get stream info
  info <- mc_get_stream_info(conn, stream_open)
  expect_equal(info$name, stream_open)

  # 4. mc_list_streams ---------------------------------------------------------
  all_streams <- mc_list_streams(conn)
  expect_s3_class(all_streams, "data.frame")
  expect_true(stream_open %in% all_streams$name)
  expect_true(stream_restricted %in% all_streams$name)
  
  # 5. mc_publish --------------------------------------------------------------
  # Publish simple text
  tx_pub_1 <- mc_publish(conn, stream_open, "key1", list(text = "Hello MultiChain"))
  expect_type(tx_pub_1, "character")
  
  # 6. mc_publish_from ---------------------------------------------------------
  # Publish JSON data from specific address
  json_data <- list(status = "active", value = 42)
  tx_pub_2 <- mc_publish_from(conn, admin_addr, stream_open, "key2", list(json = json_data))
  expect_type(tx_pub_2, "character")
  
  # 7. mc_publish_multi --------------------------------------------------------
  # Publish multiple items in one transaction
  multi_items <- list(
    list(key = "multi1", data = list(text = "Part A")),
    list(key = "multi2", data = list(json = list(part = "B")))
  )
  tx_pub_multi <- mc_publish_multi(conn, stream_open, multi_items)
  expect_type(tx_pub_multi, "character")
  
  mc_wait_for_confirmation(conn, tx_pub_1)
  mc_wait_for_confirmation(conn, tx_pub_multi)
  
  # 8. mc_publish_multi_from ---------------------------------------------------
  tx_pub_multi_from <- mc_publish_multi_from(conn, admin_addr, stream_open, multi_items)
  mc_wait_for_confirmation(conn, tx_pub_multi_from)
  
  expect_type(tx_pub_multi_from, "character")
  expect_equal(nchar(tx_pub_multi_from), 64)
  
  items_in_tx <- mc_list_stream_tx_items(conn, stream_open, tx_pub_multi_from)
  
  expect_s3_class(items_in_tx, "data.frame")
  expect_equal(nrow(items_in_tx), length(multi_items))
  
  expect_true(all(sapply(items_in_tx$publishers, function(p) admin_addr %in% p)))
  
  sent_keys <- sapply(multi_items, function(x) x$key)
  expect_true(all(sent_keys %in% unlist(items_in_tx$keys)))
  
  # 9. mc_get_stream_publisher_summary -----------------------------------------
  tx_p1 <- mc_publish_from(conn, admin_addr, stream_open, "meta_1", 
                           list(json = list(user_role = "admin", login_count = 1)))
  
  tx_p2 <- mc_publish_from(conn, admin_addr, stream_open, "meta_2", 
                           list(json = list(login_count = 2, last_login = "2026-04-03")))
  
  mc_wait_for_confirmation(conn, tx_p2)
  Sys.sleep(2)
  
  mc_subscribe(conn, stream_open)
  summary_pub <- mc_get_stream_publisher_summary(conn, stream_open, admin_addr,
                                                 mode = "jsonobjectmerge,ignoreother")
  
  expect_type(summary_pub, "list")
  expect_null(summary_pub$json)
  expect_equal(summary_pub$user_role, "admin")
  expect_equal(summary_pub$login_count, 2)
  expect_equal(summary_pub$last_login, "2026-04-03")
  expect_type(summary_pub, "list")
  expect_null(summary_pub$json) 
  expect_equal(summary_pub$user_role, "admin")
  expect_equal(summary_pub$login_count, 2)
  expect_equal(summary_pub$last_login, "2026-04-03")
  
  # 10. mc_list_stream_items ---------------------------------------------------
  # List all items
  mc_subscribe(conn, stream_open)
  items <- mc_list_stream_items(conn, stream_open, count = 10)
  expect_s3_class(items, "data.frame")
  expect_true(nrow(items) >= 4)
  
  # 11. mc_get_stream_item -----------------------------------------------------
  # Get a specific item
  single_item <- mc_get_stream_item(conn, stream_open, tx_pub_1)
  expect_equal(single_item$key[[1]], "key1")
  
  # 12. mc_list_stream_keys ----------------------------------------------------
  # List unique keys
  keys_df <- mc_list_stream_keys(conn, stream_open)
  expect_true("key1" %in% keys_df$key)
  expect_true("key2" %in% keys_df$key)
  
  # 13. mc_list_stream_key_items -----------------------------------------------
  # List items by key
  key1_items <- mc_list_stream_key_items(conn, stream_open, "key1")
  expect_equal(nrow(key1_items), 1)
  
  # 14. mc_list_stream_publishers ----------------------------------------------
  # List publishers
  publishers <- mc_list_stream_publishers(conn, stream_open)
  expect_true(admin_addr %in% publishers$publisher)
  
  # 15. mc_list_stream_publisher_items -----------------------------------------
  items_a <- mc_list_stream_publisher_items(conn, stream_open, admin_addr)
  
  expect_s3_class(items_a, "data.frame")
  #expect_equal(nrow(items_a), 7)
  expect_true(all(sapply(items_a$publishers, function(p) admin_addr %in% p)))
  
  items_b <- mc_list_stream_publisher_items(conn, stream_open, user_addr)
  
  expect_s3_class(items_b, "data.frame")
  #expect_equal(nrow(items_b), 2)
  expect_true(all(sapply(items_b$publishers, function(p) user_addr %in% p)))
  
  items_a_limited <- mc_list_stream_publisher_items(conn, stream_open, admin_addr, count = 1)
  expect_equal(nrow(items_a_limited), 1)
  expect_true(all(sapply(items_a_limited$publishers, function(p) admin_addr %in% p)))
  
  # 16. mc_get_stream_key_summary ----------------------------------------------
  # Aggregate JSON data for key2
  # We publish another update to key2 to test merging
  mc_publish(conn, stream_open, "key2", list(json = list(tags = c("test", "integration"))))
  Sys.sleep(3) # Wait for processing
  
  # Get summary for key2 (merged JSON)
  summary <- mc_get_stream_key_summary(conn, stream_open, "key2")
  expect_type(summary, "list")
  expect_equal(summary$status, "active") # From first publish
  expect_true("integration" %in% summary$tags) # From second publish
  
  # 17. mc_list_stream_query_items ---------------------------------------------
  expect_true(mc_list_streams(conn, stream_open)$subscribed)
  
  # Ensure items are published and confirmed in a block
  tx1 <- mc_publish(conn, stream_open, "key1", list(text = "query test 1"))
  tx2 <- mc_publish(conn, stream_open, "key2", list(text = "query test 2"))
  mc_wait_for_confirmation(conn, tx1)
  mc_wait_for_confirmation(conn, tx2)
  
  query_single <- list(keys = "key1")
  res_single <- mc_list_stream_query_items(conn, stream_open, query_single)
  
  expect_true(nrow(res_single) >= 1)
  expect_equal(unlist(res_single$key[1]), "key1")
  
  admin_addr <- mc_get_addresses(conn)[1]
  query_pub <- list(publishers = admin_addr)
  res_pub <- mc_list_stream_query_items(conn, stream_open, query_pub)
  
  expect_true(nrow(res_pub) >= 1)

  # 18. mc_list_stream_tx_items ------------------------------------------------
  # List items in a specific transaction (the multi-publish tx)
  tx_items <- mc_list_stream_tx_items(conn, stream_open, tx_pub_multi)
  expect_equal(nrow(tx_items), 2)
  expect_true(all(c("multi1", "multi2") %in% unlist(tx_items$keys)))
  
  # 19. mc_list_stream_block_items ---------------------------------------------
  tx_block_test <- mc_publish(conn, stream_open, "block_key", list(text = "block data"))
  mc_wait_for_confirmation(conn, tx_block_test)
  
  tx_info <- mc_get_wallet_transaction(conn, tx_block_test)
  actual_height <- tx_info$blockheight
  
  block_items <- mc_list_stream_block_items(conn, stream_open, actual_height)
  
  expect_s3_class(block_items, "data.frame")
  expect_true(nrow(block_items) > 0)
  expect_true(tx_block_test %in% block_items$txid)
})
