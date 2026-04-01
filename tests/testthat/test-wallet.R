conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_list_unspent returns data.frame with assets", {
  fake_body <- '{"result":[{"txid":"tx1","vout":0,"address":"1A","amount":10,"assets":[{"name":"ast","qty":5}]}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200, 
        headers = list("Content-Type" = "application/json"), 
        body = charToRaw(fake_body)
      )
    },
    {
      df <- mc_list_unspent(conn_mock)
      
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 1)
      expect_equal(df$txid[1], "tx1")
      
      # Теперь assets[[1]] будет списком, содержащим один элемент (информацию об активе)
      expect_type(df$assets[[1]], "list")
      expect_equal(df$assets[[1]][[1]]$name, "ast")
    }
  )
})

test_that("mc_combine_unspent returns txid vector", {
  fake_body <- '{"result":["tx_comb_1","tx_comb_2"],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_combine_unspent(conn_mock)
      expect_type(res, "character")
      expect_equal(length(res), 2)
      expect_equal(res[1], "tx_comb_1")
    }
  )
})

test_that("mc_lock_unspent returns boolean success", {
  fake_body <- '{"result":true,"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      # Лочим конкретный выход
      res <- mc_lock_unspent(conn_mock, unlock = FALSE, outputs = list(list(txid = "tx1", vout = 0)))
      expect_true(res)
    }
  )
})

test_that("mc_list_lock_unspent returns empty df when nothing locked", {
  fake_body <- '{"result":[],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      df <- mc_list_lock_unspent(conn_mock)
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 0)
    }
  )
})

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

test_that("mc_list_wallet_transactions returns data.frame", {
  fake_body <- '{"result":[{"txid":"tx1","amount":1.5},{"txid":"tx2","amount":-0.5}],"error":null,"id":1}'
  
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
      expect_equal(df$txid[2], "tx2")
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

test_that("mc_dump_privkey returns the key string", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  # Mock response for dumpprivkey
  fake_response <- list(
    result = "Vp4k8S...dummy_wif_key...",
    error = NULL,
    id = 1
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(as.character(jsonlite::toJSON(fake_response, null = "null", auto_unbox = TRUE)))
      )
    },
    {
      key <- mc_dump_privkey(conn, "1ABC...")
      expect_equal(key, "Vp4k8S...dummy_wif_key...")
    }
  )
})

test_that("mc_get_wallet_info returns wallet status", {
  fake_body <- '{"result":{"walletversion":60000,"balance":10.5,"txcount":42},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, headers = list("Content-Type" = "application/json"), body = charToRaw(fake_body))
    },
    {
      res <- mc_get_wallet_info(conn_mock)
      expect_type(res, "list")
      expect_equal(res$txcount, 42)
      expect_equal(res$balance, 10.5)
    }
  )
})

test_that("mc_dump_privkey returns the key string", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_response <- list(
    result = NULL,
    error = NULL,
    id = 1
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(as.character(jsonlite::toJSON(fake_response, null = "null", auto_unbox = TRUE)))
      )
    },
    {
      # Should not throw error and return NULL
      res <- mc_dump_privkey(conn, "Vp4k8S...")
      expect_null(res)
    }
  )
  fake_body <- '{"result":"Vp4k8S...","error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_dump_privkey(conn_mock, "1ADDR")
      expect_equal(res, "Vp4k8S...")
    }
  )
})

test_that("mc_unlock_wallet returns NULL on success", {
  fake_body <- '{"result":null,"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_unlock_wallet(conn_mock, "secret_pass", 60)
      expect_null(res)
    }
  )
})

test_that("mc_import_privkey handles vector of keys", {
  fake_body <- '{"result":null,"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_import_privkey(conn_mock, c("key1", "key2"), rescan = FALSE)
      expect_null(res)
    }
  )
})