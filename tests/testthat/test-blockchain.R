conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_get_block works with numeric height", {
  fake_block <- list(hash = "000...", height = 123)
  fake_body <- jsonlite::toJSON(list(result = fake_block), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_block(conn_mock, 123)
      expect_equal(res$height, 123)
    }
  )
})

test_that("mc_get_block works with string hash", {
  fake_block <- list(hash = "abc...", height = 456)
  fake_body <- jsonlite::toJSON(list(result = fake_block), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_block(conn_mock, "abc...")
      expect_equal(res$hash, "abc...")
    }
  )
})

test_that("mc_get_blockchain_info returns list", {
  fake_body <- '{"result":{"chain":"test","blocks":100,"bestblockhash":"abc"},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, headers = list("Content-Type" = "application/json"), body = charToRaw(fake_body))
    },
    {
      res <- mc_get_blockchain_info(conn_mock)
      expect_type(res, "list")
      expect_equal(res$blocks, 100)
    }
  )
})

test_that("mc_get_block_hash returns string", {
  fake_body <- '{"result":"hash123","error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_get_block_hash(conn_mock, 10)
      expect_equal(res, "hash123")
    }
  )
})

test_that("mc_get_chain_totals returns totals", {
  fake_totals <- list(addresses = 100, transactions = 500)
  fake_body <- jsonlite::toJSON(list(result = fake_totals), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_chain_totals(conn_mock)
      expect_equal(res$addresses, 100)
    }
  )
})

test_that("mc_get_last_block_info returns block info", {
  fake_block <- list(height = 1000, hash = "last_hash")
  fake_body <- jsonlite::toJSON(list(result = fake_block), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_last_block_info(conn_mock, skip = 5)
      expect_equal(res$height, 1000)
    }
  )
})

test_that("mc_get_mempool_info returns mempool stats", {
  fake_mempool <- list(size = 10, bytes = 5000)
  fake_body <- jsonlite::toJSON(list(result = fake_mempool), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_mempool_info(conn_mock)
      expect_equal(res$size, 10)
    }
  )
})

test_that("mc_list_blocks returns data.frame", {
  fake_body <- '{"result":[{"height":10,"hash":"h10"},{"height":11,"hash":"h11"}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      df <- mc_list_blocks(conn_mock, "-2")
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 2)
    }
  )
})

test_that("mc_get_raw_mempool returns vector", {
  fake_body <- '{"result":["tx1","tx2"],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_get_raw_mempool(conn_mock)
      expect_type(res, "character")
      expect_equal(length(res), 2)
    }
  )
})

test_that("mc_get_raw_transaction returns tx hex when verbose=FALSE", {
  fake_hex <- "01020304"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_hex)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_raw_transaction(conn_mock, "txid123")
      expect_equal(res, fake_hex)
    }
  )
})

test_that("mc_get_raw_transaction returns decoded tx when verbose=TRUE", {
  fake_tx <- list(txid = "abc", vout = 0)
  fake_body <- jsonlite::toJSON(list(result = fake_tx), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_raw_transaction(conn_mock, "txid123", verbose = TRUE)
      expect_equal(res$txid, "abc")
    }
  )
})

test_that("mc_get_tx_out returns UTXO info", {
  fake_utxo <- list(confirmations = 10, value = 1.5)
  fake_body <- jsonlite::toJSON(list(result = fake_utxo), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_tx_out(conn_mock, "txid123", vout = 0, unconfirmed = TRUE)
      expect_equal(res$value, 1.5)
    }
  )
})

test_that("mc_list_miners returns data frame", {
  fake_miners <- list(list(address = "1A...", status = "active"))
  fake_body <- jsonlite::toJSON(list(result = fake_miners), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_miners(conn_mock, verbose = TRUE)
      expect_s3_class(res, "data.frame")
      expect_equal(res$address, "1A...")
    }
  )
})