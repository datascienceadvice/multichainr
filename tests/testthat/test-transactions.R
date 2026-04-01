conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_get_address_balances returns data.frame", {
  fake_body <- '{"result":[{"name":"asset1","qty":100}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      df <- mc_get_address_balances(conn_mock, "1ADDR")
      expect_s3_class(df, "data.frame")
      expect_equal(df$qty[1], 100)
    }
  )
})

test_that("mc_get_address_transaction returns transaction details as list", {
  fake_body <- '{"result":{"txid":"tx123","amount":-10.5,"confirmations":5},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_get_address_transaction(conn_mock, "1ADDR", "tx123")
      expect_type(res, "list")
      expect_equal(res$txid, "tx123")
      expect_equal(res$amount, -10.5)
    }
  )
})

test_that("mc_get_multi_balances returns a complex list", {
  # getmultibalances возвращает объект, где ключи - адреса
  fake_body <- '{"result":{"1ADDR":[{"name":"asset1","qty":50}],"1OTHER":[{"name":"asset1","qty":20}]},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_get_multi_balances(conn_mock, addresses = "*")
      expect_type(res, "list")
      expect_true("1ADDR" %in% names(res))
      expect_equal(res[["1ADDR"]][[1]]$qty, 50)
    }
  )
})

test_that("mc_get_token_balances returns data.frame of NFT tokens", {
  fake_body <- '{"result":[{"name":"NFT_Asset","token":"token1","qty":1}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      df <- mc_get_token_balances(conn_mock)
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 1)
      expect_equal(df$token[1], "token1")
    }
  )
})

test_that("mc_get_total_balances returns data.frame of all assets", {
  fake_body <- '{"result":[{"name":"assetA","qty":1000},{"name":"assetB","qty":50.5}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      df <- mc_get_total_balances(conn_mock)
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 2)
      expect_equal(df$qty[1], 1000)
    }
  )
})

test_that("mc_get_wallet_transaction returns list", {
  fake_body <- '{"result":{"txid":"tx123","confirmations":10},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_get_wallet_transaction(conn_mock, "tx123")
      expect_type(res, "list")
      expect_equal(res$confirmations, 10)
    }
  )
})

test_that("mc_list_address_transactions returns data.frame of history", {
  fake_body <- '{"result":[{"txid":"txA","time":1600000000},{"txid":"txB","time":1600000001}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      df <- mc_list_address_transactions(conn_mock, "1ADDR", count = 2)
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 2)
      expect_equal(df$txid[2], "txB")
    }
  )
})

test_that("mc_list_wallet_transactions returns data.frame of wallet history", {
  fake_body <- '{"result":[{"txid":"txW1","comment":"test"},{"txid":"txW2","comment":"second"}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      df <- mc_list_wallet_transactions(conn_mock, count = 2)
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 2)
      expect_equal(df$comment[1], "test")
    }
  )
})

test_that("mc_get_tx_out_data returns hex string", {
  fake_hex <- "48656c6c6f20576f726c64" # "Hello World"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_hex)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_get_tx_out_data(conn_mock, "txid123", 0)
      expect_type(res, "character")
      expect_equal(res, fake_hex)
    }
  )
})