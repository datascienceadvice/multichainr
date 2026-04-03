skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Binary Cache lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_cache_", round(as.numeric(Sys.time())))
  
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
  
  # 1. mc_create_binary_cache --------------------------------------------------
  # Create a new empty cache item
  cache_id <- mc_create_binary_cache(conn)
  expect_type(cache_id, "character")
  expect_true(nchar(cache_id) > 0)
  
  # 2. mc_append_binary_cache --------------------------------------------------
  # Append plain text
  size1 <- mc_append_binary_cache(conn, cache_id, list(text = "Hello"))
  expect_true(size1 >= 5)
  
  # Append JSON data
  size2 <- mc_append_binary_cache(conn, cache_id, list(json = list(a = 1, b = 2)))
  expect_true(size2 > size1)
  
  # Append raw hex string
  size3 <- mc_append_binary_cache(conn, cache_id, "00ffaa")
  expect_equal(size3, size2 + 3)
  
  # Check current size without appending
  size_final <- mc_append_binary_cache(conn, cache_id, "")
  expect_equal(size_final, size3)
  
  # Prepare Blockchain Data ----------------------------------------------------
  # We need an actual transaction output to test mc_txout_to_binary_cache.
  # We create a stream and publish an item with data.
  stream_name <- "cache_test_stream"
  mc_create_stream(conn, stream_name, open = TRUE)
  
  binary_hex <- "0102030405060708090a0b0c0d0e0f"
  pub_txid <- mc_publish(conn, stream_name, "bin_key", binary_hex)
  expect_type(pub_txid, "character")
  
  # 3. mc_txout_to_binary_cache ------------------------------------------------
  # Create a fresh empty cache item for extraction
  extract_cache_id <- mc_create_binary_cache(conn)
  
  # Copy the raw binary data from the transaction output to the cache.
  # For stream items, the data is usually in vout 0.
  extract_size <- mc_txout_to_binary_cache(conn, extract_cache_id, pub_txid, vout = 0)
  
  expect_type(extract_size, "integer")
  expect_true(extract_size > 0)
  
  # Verify size matches our binary_hex (length of hex / 2)
  expect_equal(extract_size, nchar(binary_hex) / 2)
  
  # 4. mc_delete_binary_cache --------------------------------------------------
  # Clean up the cache items from the node's local storage
  res1 <- mc_delete_binary_cache(conn, cache_id)
  expect_null(res1)
  
  res2 <- mc_delete_binary_cache(conn, extract_cache_id)
  expect_null(res2)
  
  # Verify deletion by attempting to append to a non-existent item (should fail)
  expect_error(mc_append_binary_cache(conn, cache_id, "data"), "not found")
})
