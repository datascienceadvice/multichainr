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

test_that("mc_create_raw_transaction works with empty data", {
  fake_tx_hex <- "01020304"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_tx_hex)
  inputs <- list(list(txid = "abc", vout = 0))
  outputs <- list("1A..." = 1.0)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_raw_transaction(conn_mock, inputs, outputs)
      expect_equal(res, fake_tx_hex)
    }
  )
})

test_that("mc_create_raw_transaction works with non-empty data (string)", {
  fake_tx_hex <- "01020304"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_tx_hex)
  inputs <- list(list(txid = "abc", vout = 0))
  outputs <- list("1A..." = 1.0)
  data <- list("simple text")
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_raw_transaction(conn_mock, inputs, outputs, data = data)
      expect_equal(res, fake_tx_hex)
    }
  )
})

test_that("mc_create_raw_transaction works with non-empty data (list)", {
  fake_tx_hex <- "01020304"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_tx_hex)
  inputs <- list(list(txid = "abc", vout = 0))
  outputs <- list("1A..." = 1.0)
  data <- list(list(key = "value"))
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_raw_transaction(conn_mock, inputs, outputs, data = data)
      expect_equal(res, fake_tx_hex)
    }
  )
})

test_that("mc_create_raw_send_from works with empty data", {
  fake_tx_hex <- "01020304"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_tx_hex)
  to_amounts <- list("1B..." = 0.5)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_raw_send_from(conn_mock, "from_addr", to_amounts)
      expect_equal(res, fake_tx_hex)
    }
  )
})

test_that("mc_create_raw_send_from works with data as list of strings", {
  fake_tx_hex <- "01020304"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_tx_hex)
  to_amounts <- list("1B..." = 0.5)
  data <- list("metadata1", "metadata2")
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_raw_send_from(conn_mock, "from_addr", to_amounts, data = data)
      expect_equal(res, fake_tx_hex)
    }
  )
})

test_that("mc_create_raw_send_from works with data containing nested list (JSON conversion)", {
  fake_tx_hex <- "01020304"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_tx_hex)
  to_amounts <- list("1B..." = 0.5)
  data <- list(list(key = "value"), "simple")
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_raw_send_from(conn_mock, "from_addr", to_amounts, data = data)
      expect_equal(res, fake_tx_hex)
    }
  )
})

test_that("mc_append_raw_change works without native_fee", {
  fake_tx_hex <- "01020304"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_tx_hex)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_append_raw_change(conn_mock, "old_hex", "1A...")
      expect_equal(res, fake_tx_hex)
    }
  )
})

test_that("mc_append_raw_change works with native_fee", {
  fake_tx_hex <- "01020304"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_tx_hex)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_append_raw_change(conn_mock, "old_hex", "1A...", native_fee = 0.001)
      expect_equal(res, fake_tx_hex)
    }
  )
})

test_that("mc_append_raw_data works with string data", {
  fake_tx_hex <- "01020304"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_tx_hex)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_append_raw_data(conn_mock, "old_hex", "my data")
      expect_equal(res, fake_tx_hex)
    }
  )
})

test_that("mc_append_raw_data works with list data", {
  fake_tx_hex <- "01020304"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_tx_hex)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_append_raw_data(conn_mock, "old_hex", list(a = 1, b = 2))
      expect_equal(res, fake_tx_hex)
    }
  )
})

test_that("mc_append_raw_transaction works with empty inputs and outputs", {
  fake_tx_hex <- "01020304"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_tx_hex)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_append_raw_transaction(conn_mock, "old_hex")
      expect_equal(res, fake_tx_hex)
    }
  )
})

test_that("mc_append_raw_transaction works with non-empty inputs and outputs", {
  fake_tx_hex <- "01020304"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_tx_hex)
  inputs <- list(list(txid = "abc", vout = 0))
  outputs <- list("1B..." = 0.5)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_append_raw_transaction(conn_mock, "old_hex", inputs, outputs)
      expect_equal(res, fake_tx_hex)
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

test_that("mc_send_raw_transaction works", {
  fake_txid <- "sent_txid"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_send_raw_transaction(conn_mock, "signed_hex")
      expect_equal(res, fake_txid)
    }
  )
})
