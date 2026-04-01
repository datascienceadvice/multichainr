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
