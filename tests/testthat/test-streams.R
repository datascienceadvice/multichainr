conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_create_stream returns txid", {
  fake_txid <- "stream_txid_123"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      tx1 <- mc_create_stream(conn_mock, "test_stream", open = TRUE)
      expect_equal(tx1, fake_txid)
      
      tx2 <- mc_create_stream(conn_mock, "restricted", open = list(restrict = "write"))
      expect_equal(tx2, fake_txid)
    }
  )
})

test_that("mc_create_stream works with open = TRUE (logical) and no custom_fields", {
  fake_txid <- "stream_txid_001"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_stream(conn_mock, "mystream", open = TRUE)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_create_stream works with open = FALSE (logical) and no custom_fields", {
  fake_txid <- "stream_txid_002"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_stream(conn_mock, "mystream", open = FALSE)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_create_stream works with open as list (restricted) and no custom_fields", {
  fake_txid <- "stream_txid_003"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      open_list <- list(restrict = "write")
      res <- mc_create_stream(conn_mock, "mystream", open = open_list)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_create_stream works with custom_fields", {
  fake_txid <- "stream_txid_004"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      custom <- list(owner = "admin", version = 1)
      res <- mc_create_stream(conn_mock, "mystream", open = TRUE, custom_fields = custom)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_create_stream works with open as list and custom_fields", {
  fake_txid <- "stream_txid_005"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      open_list <- list(restrict = "write", details = "something")
      custom <- list(field = "value")
      res <- mc_create_stream(conn_mock, "mystream", open = open_list, custom_fields = custom)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_create_stream_from works without custom_fields", {
  fake_txid <- "create_stream_txid_001"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_stream_from(conn_mock, "from_addr", "mystream", open = TRUE)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_create_stream_from works with custom_fields", {
  fake_txid <- "create_stream_txid_002"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_stream_from(conn_mock, "from_addr", "mystream",
                                   open = FALSE, custom_fields = list(owner = "admin"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_get_stream_info returns list", {
  fake_body <- '{"result":{"name":"mystream","streamref":"1-2-3","items":10},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_get_stream_info(conn_mock, "mystream")
      expect_type(res, "list")
      expect_equal(res$streamref, "1-2-3")
      expect_equal(res$items, 10)
    }
  )
})

test_that("mc_list_streams returns data.frame", {
  fake_body <- '{"result":[{"name":"s1","items":5},{"name":"s2","items":0}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      df <- mc_list_streams(conn_mock)
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 2)
      expect_equal(df$name[1], "s1")
    }
  )
})

test_that("mc_list_streams works with default parameters", {
  fake_streams <- list(list(name = "stream1", ref = "ref1"))
  fake_body <- jsonlite::toJSON(list(result = fake_streams), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_streams(conn_mock)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_streams works with count parameter", {
  fake_streams <- list(list(name = "stream1", ref = "ref1"))
  fake_body <- jsonlite::toJSON(list(result = fake_streams), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_streams(conn_mock, count = 5)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_streams works with start parameter (and no count)", {
  fake_streams <- list(list(name = "stream1", ref = "ref1"))
  fake_body <- jsonlite::toJSON(list(result = fake_streams), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_streams(conn_mock, start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_streams works with both count and start", {
  fake_streams <- list(list(name = "stream1", ref = "ref1"))
  fake_body <- jsonlite::toJSON(list(result = fake_streams), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_streams(conn_mock, count = 5, start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_publish and mc_publish_from return txid", {
  fake_txid <- "pub_txid_123"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      tx1 <- mc_publish(conn_mock, "stream1", "key1", list(text = "hello"))
      expect_equal(tx1, fake_txid)
      
      tx2 <- mc_publish(conn_mock, "stream1", c("k1", "k2"), "68656c6c6f") # hex "hello"
      expect_equal(tx2, fake_txid)
      
      tx3 <- mc_publish_from(conn_mock, "1ADDR", "stream1", "key1", list(json = list(a = 1)))
      expect_equal(tx3, fake_txid)
    }
  )
})

test_that("mc_publish works with single key and no options", {
  fake_txid <- "publish_txid_001"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish(conn_mock, "mystream", "key1", list(text = "data"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish works with multiple keys", {
  fake_txid <- "publish_txid_002"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish(conn_mock, "mystream", c("key1", "key2"), list(text = "data"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish works with options", {
  fake_txid <- "publish_txid_003"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish(conn_mock, "mystream", "key1", list(text = "data"), options = "offchain")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish handles single key and character data (not hex)", {
  fake_txid <- "publish_txid_001"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish(conn_mock, "mystream", "key1", "Hello, world!")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish handles single key and character data that is already hex", {
  fake_txid <- "publish_txid_002"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  hex_data <- "48656c6c6f"  # "Hello"
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish(conn_mock, "mystream", "key1", hex_data)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish handles data starting with 'cache-' (cacheitem)", {
  fake_txid <- "publish_txid_003"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish(conn_mock, "mystream", "key1", "cache-123")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish handles multiple keys", {
  fake_txid <- "publish_txid_004"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish(conn_mock, "mystream", c("key1", "key2"), "data")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish handles list data (no transformation)", {
  fake_txid <- "publish_txid_005"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish(conn_mock, "mystream", "key1", list(text = "data"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish includes options when provided", {
  fake_txid <- "publish_txid_006"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish(conn_mock, "mystream", "key1", "data", options = "offchain")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish converts cache list to string", {
  fake_txid <- "publish_cache_txid"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res1 <- mc_publish(conn_mock, "mystream", "key1", list(cacheitem = "cache-123"))
      expect_equal(res1, fake_txid)
      
      res2 <- mc_publish(conn_mock, "mystream", "key2", list(`cache-item` = "cache-456"))
      expect_equal(res2, fake_txid)
    }
  )
})

test_that("mc_publish_from works without options", {
  fake_txid <- "publish_from_txid_001"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish_from(conn_mock, "from_addr", "mystream", "key1", list(text = "data"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish_from works with options", {
  fake_txid <- "publish_from_txid_002"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish_from(conn_mock, "from_addr", "mystream", "key1",
                             list(text = "data"), options = "offchain")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish_from works with single key, non-cache data, no options", {
  fake_txid <- "publish_from_txid_001"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish_from(conn_mock, "from_addr", "mystream", "key1", "data")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish_from works with multiple keys and cache data", {
  fake_txid <- "publish_from_txid_002"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish_from(conn_mock, "from_addr", "mystream", c("key1", "key2"), "cache-123")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish_from works with single key, non-cache data, and options", {
  fake_txid <- "publish_from_txid_003"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish_from(conn_mock, "from_addr", "mystream", "key1", "data", options = "offchain")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish_from works with list data (not character)", {
  fake_txid <- "publish_from_txid_004"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish_from(conn_mock, "from_addr", "mystream", "key1", list(text = "data"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish_from converts cache list to string", {
  fake_txid <- "publish_from_cache_txid"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res1 <- mc_publish_from(conn_mock, "from_addr", "mystream", "key1", 
                              data = list(cacheitem = "cache-123"))
      expect_equal(res1, fake_txid)
      
      res2 <- mc_publish_from(conn_mock, "from_addr", "mystream", "key2", 
                              data = list(`cache-item` = "cache-456"))
      expect_equal(res2, fake_txid)
    }
  )
})

test_that("mc_publish_multi returns txid", {
  fake_txid <- "multi_txid_456"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  items <- list(
    list(key = "k1", data = list(text = "item1")),
    list(keys = c("k2", "k3"), data = list(json = list(val = 100)))
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      tx <- mc_publish_multi(conn_mock, "stream1", items)
      expect_equal(tx, fake_txid)
    }
  )
})

test_that("mc_publish_multi works without options", {
  fake_txid <- "publish_multi_txid_001"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  items <- list(list(key = "k1", data = list(text = "d1")))
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish_multi(conn_mock, "mystream", items)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish_multi works with options", {
  fake_txid <- "publish_multi_txid_002"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  items <- list(list(key = "k1", data = list(text = "d1")))
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish_multi(conn_mock, "mystream", items, options = "offchain")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish_multi_from works without options", {
  fake_txid <- "publish_multi_from_txid_001"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  items <- list(list(key = "k1", data = list(text = "d1")))
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish_multi_from(conn_mock, "from_addr", "mystream", items)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish_multi_from works with options", {
  fake_txid <- "publish_multi_from_txid_002"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  items <- list(list(key = "k1", data = list(text = "d1")))
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish_multi_from(conn_mock, "from_addr", "mystream", items, options = "offchain")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish_multi works with items that have regular data (no cache)", {
  fake_txid <- "publish_multi_txid_001"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  items <- list(
    list(key = "k1", data = "Hello"),
    list(key = "k2", data = list(json = list(a = 1)))
  )
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish_multi(conn_mock, "mystream", items)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish_multi works with items where data starts with 'cache-'", {
  fake_txid <- "publish_multi_txid_002"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  items <- list(
    list(key = "k1", data = "cache-123"),
    list(key = "k2", data = "regular")
  )
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish_multi(conn_mock, "mystream", items)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_publish_multi works with options parameter", {
  fake_txid <- "publish_multi_txid_003"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  items <- list(list(key = "k1", data = "data"))
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_publish_multi(conn_mock, "mystream", items, options = "offchain")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_get_stream_item returns a single item list", {
  fake_body <- '{"result":{"publishers":["addr1"],"key":"k1","data":"6162"},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_get_stream_item(conn_mock, "s1", "tx123")
      expect_type(res, "list")
      expect_equal(res$key, "k1")
    }
  )
})

test_that("mc_get_stream_key_summary returns merged JSON list", {
  fake_body <- '{"result":{"status":"active","priority":10,"user":"admin"},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_get_stream_key_summary(conn_mock, "s1", "order123")
      expect_type(res, "list")
      expect_equal(res$status, "active")
      expect_equal(res$priority, 10)
    }
  )
})

test_that("mc_get_stream_key_summary strips the 'json' wrapper", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_body <- '{"result":{"json":{"status":"active","value":42}},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_get_stream_key_summary(conn, "mystream", "key1")

      expect_type(res, "list")
      expect_equal(res$status, "active")
      expect_equal(res$value, 42)
      expect_null(res$json)
    }
  )
})

test_that("mc_get_stream_publisher_summary works", {
  fake_summary <- list(address = "1A...", items = 5)
  fake_body <- jsonlite::toJSON(list(result = fake_summary), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_stream_publisher_summary(conn_mock, "mystream", "1A...")
      expect_equal(res$address, "1A...")
    }
  )
})

test_that("mc_get_stream_publisher_summary strips the 'json' wrapper", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")

  fake_body <- '{"result":{"json":{"user_role":"admin","active":true}},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_get_stream_publisher_summary(conn, "mystream", "1ADDR")
      
      expect_type(res, "list")
      expect_equal(res$user_role, "admin")
      expect_null(res$json)
    }
  )
})

test_that("mc_list_stream_items works with default start = NULL", {
  fake_items <- list(list(txid = "tx1", key = "k1"))
  fake_body <- jsonlite::toJSON(list(result = fake_items), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_items(conn_mock, "mystream")
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_items works with start parameter", {
  fake_items <- list(list(txid = "tx1", key = "k1"))
  fake_body <- jsonlite::toJSON(list(result = fake_items), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_items(conn_mock, "mystream", start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_keys returns data.frame of keys", {
  fake_body <- '{"result":[{"key":"k1","items":5},{"key":"k2","items":12}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      df <- mc_list_stream_keys(conn_mock, "s1")
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 2)
      expect_equal(df$items[2], 12)
    }
  )
})

test_that("mc_list_stream_key_items works with default start = NULL", {
  fake_items <- list(list(txid = "tx1", key = "k1"))
  fake_body <- jsonlite::toJSON(list(result = fake_items), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_key_items(conn_mock, "mystream", "k1")
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_key_items works with start parameter", {
  fake_items <- list(list(txid = "tx1", key = "k1"))
  fake_body <- jsonlite::toJSON(list(result = fake_items), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_key_items(conn_mock, "mystream", "k1", start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_keys works with default parameters", {
  fake_keys <- list(list(key = "k1", count = 5))
  fake_body <- jsonlite::toJSON(list(result = fake_keys), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_keys(conn_mock, "mystream")
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_keys works with count parameter", {
  fake_keys <- list(list(key = "k1", count = 5))
  fake_body <- jsonlite::toJSON(list(result = fake_keys), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_keys(conn_mock, "mystream", count = 5)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_keys works with start parameter (and no count)", {
  fake_keys <- list(list(key = "k1", count = 5))
  fake_body <- jsonlite::toJSON(list(result = fake_keys), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_keys(conn_mock, "mystream", start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_keys works with both count and start", {
  fake_keys <- list(list(key = "k1", count = 5))
  fake_body <- jsonlite::toJSON(list(result = fake_keys), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_keys(conn_mock, "mystream", count = 5, start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_keys works with local_ordering = TRUE", {
  fake_keys <- list(list(key = "k1", count = 5))
  fake_body <- jsonlite::toJSON(list(result = fake_keys), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_keys(conn_mock, "mystream", local_ordering = TRUE)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_publisher_items works with default start = NULL", {
  fake_items <- list(list(txid = "tx1", key = "k1"))
  fake_body <- jsonlite::toJSON(list(result = fake_items), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_publisher_items(conn_mock, "mystream", "1A...")
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_publisher_items works with start parameter", {
  fake_items <- list(list(txid = "tx1", key = "k1"))
  fake_body <- jsonlite::toJSON(list(result = fake_items), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_publisher_items(conn_mock, "mystream", "1A...", start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_publishers works with default parameters", {
  fake_publishers <- list(list(address = "1A...", items = 5))
  fake_body <- jsonlite::toJSON(list(result = fake_publishers), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_publishers(conn_mock, "mystream")
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_publishers works with count parameter", {
  fake_publishers <- list(list(address = "1A...", items = 5))
  fake_body <- jsonlite::toJSON(list(result = fake_publishers), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_publishers(conn_mock, "mystream", count = 5)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_publishers works with start parameter (and no count)", {
  fake_publishers <- list(list(address = "1A...", items = 5))
  fake_body <- jsonlite::toJSON(list(result = fake_publishers), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_publishers(conn_mock, "mystream", start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_publishers works with both count and start", {
  fake_publishers <- list(list(address = "1A...", items = 5))
  fake_body <- jsonlite::toJSON(list(result = fake_publishers), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_publishers(conn_mock, "mystream", count = 5, start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_query_items returns filtered data.frame", {
  fake_body <- '{"result":[{"key":"k1","data":"aa"},{"key":"k1","data":"bb"}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200, 
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      query <- list(keys = c("k1"), publishers = c("addr1"))
      df <- mc_list_stream_query_items(conn_mock, "s1", query)
      
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 2)
      expect_equal(df$data[1], "aa")
    }
  )
})

test_that("mc_list_stream_query_items handles NULL query", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")

  fake_body <- '{"result":[],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_list_stream_query_items(conn, "mystream", query = NULL)
      
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), 0)
    }
  )
})

test_that("mc_list_stream_query_items handles empty list query", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  fake_body <- '{"result":[],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_query_items(conn, "mystream", query = list())
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_tx_items returns data.frame", {
  fake_body <- '{"result":[{"key":"k1","data":"cc"}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200, 
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      df <- mc_list_stream_tx_items(conn_mock, "s1", "tx123")
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 1)
    }
  )
})

test_that("mc_list_stream_block_items works without count and start", {
  fake_items <- list(list(txid = "tx1", key = "k1"))
  fake_body <- jsonlite::toJSON(list(result = fake_items), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_block_items(conn_mock, "mystream", blocks = 100)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_block_items works with count parameter", {
  fake_items <- list(list(txid = "tx1", key = "k1"))
  fake_body <- jsonlite::toJSON(list(result = fake_items), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_block_items(conn_mock, "mystream", blocks = 100, count = 5)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_block_items works with start parameter", {
  fake_items <- list(list(txid = "tx1", key = "k1"))
  fake_body <- jsonlite::toJSON(list(result = fake_items), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_block_items(conn_mock, "mystream", blocks = 100, start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_stream_block_items handles both count and start", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_body <- '{"result":[],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_list_stream_block_items(
        conn, 
        stream = "mystream", 
        blocks = 1000, 
        count = 10, 
        start = 5
      )
      
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), 0)
    }
  )
})
