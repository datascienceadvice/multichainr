conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_create_binary_cache returns identifier", {
  fake_id <- "cache_item_abc123"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_id)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, headers = list("Content-Type" = "application/json"), body = charToRaw(fake_body))
    },
    {
      res <- mc_create_binary_cache(conn_mock)
      expect_equal(res, fake_id)
    }
  )
})

test_that("mc_append_binary_cache returns numeric size", {
  fake_body <- '{"result":1024,"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      size <- mc_append_binary_cache(conn_mock, "id1", "48656c6c6f")
      expect_equal(size, 1024)
    }
  )
})

test_that("mc_append_binary_cache handles text lists", {
  fake_body <- '{"result":10,"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      size <- mc_append_binary_cache(conn_mock, "id1", list(text = "Hello"))
      expect_equal(size, 10)
    }
  )
})

test_that("mc_txout_to_binary_cache returns size", {
  fake_body <- '{"result":5000,"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_txout_to_binary_cache(conn_mock, "target_id", "txid123", 0)
      expect_equal(res, 5000)
    }
  )
})

test_that("mc_delete_binary_cache returns NULL", {
  fake_body <- '{"result":null,"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_delete_binary_cache(conn_mock, "id_to_delete")
      expect_null(res)
    }
  )
})

test_that("mc_txout_to_binary_cache calls RPC with count_bytes and start_byte", {
  fake_size <- 123
  fake_body <- sprintf('{"result":%d,"error":null,"id":1}', fake_size)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_txout_to_binary_cache(
        conn = conn_mock,
        identifier = "cache_id",
        txid = "txid123",
        vout = 0,
        count_bytes = 100,
        start_byte = 5
      )
      expect_equal(res, fake_size)
    }
  )
})
