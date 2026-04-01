conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_create_raw_transaction returns hex", {
  fake_hex <- "0100000001..."
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_hex)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      inputs <- list(list(txid = "abc", vout = 0))
      outputs <- list("1ADDR" = 10)
      res <- mc_create_raw_transaction(conn_mock, inputs, outputs)
      expect_equal(res, fake_hex)
    }
  )
})

test_that("mc_decode_raw_transaction returns transaction details", {
  fake_body <- '{"result":{"txid":"tx123","version":1,"vin":[],"vout":[]},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_decode_raw_transaction(conn_mock, "some_hex")
      expect_type(res, "list")
      expect_equal(res$txid, "tx123")
    }
  )
})

test_that("mc_sign_raw_transaction returns hex and status", {
  fake_body <- '{"result":{"hex":"signed_hex","complete":true},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_sign_raw_transaction(conn_mock, "unsigned_hex")
      expect_true(res$complete)
      expect_equal(res$hex, "signed_hex")
    }
  )
})
