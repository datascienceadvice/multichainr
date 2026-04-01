conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_prepare_lock_unspent returns txid and vout", {
  fake_body <- '{"result":{"txid":"tx123","vout":0},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_prepare_lock_unspent(conn_mock, list(asset1 = 10))
      expect_type(res, "list")
      expect_equal(res$txid, "tx123")
      expect_equal(res$vout, 0)
    }
  )
})

test_that("mc_create_raw_exchange returns hex string", {
  fake_hex <- "0100000001..."
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_hex)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_create_raw_exchange(conn_mock, "tx123", 0, list(asset2 = 20))
      expect_equal(res, fake_hex)
    }
  )
})

test_that("mc_decode_raw_exchange returns structured list", {
  fake_body <- '{"result":{"offer":{"qty":10},"ask":{"qty":20},"complete":false},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_decode_raw_exchange(conn_mock, "fake_hex")
      expect_type(res, "list")
      expect_false(res$complete)
      expect_equal(res$offer$qty, 10)
    }
  )
})

test_that("mc_append_raw_exchange returns hex and complete status", {
  fake_body <- '{"result":{"hex":"new_hex","complete":true},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_append_raw_exchange(conn_mock, "old_hex", "tx_ref", 1, list(native = 1))
      expect_true(res$complete)
      expect_equal(res$hex, "new_hex")
    }
  )
})
