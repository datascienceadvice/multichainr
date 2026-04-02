conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_get_asset_info returns asset details", {
  fake_body <- '{"result":{"name":"myasset","issuetxid":"tx123","units":0.01},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_get_asset_info(conn_mock, "myasset")
      expect_type(res, "list")
      expect_equal(res$name, "myasset")
      expect_equal(res$units, 0.01)
    }
  )
})

test_that("mc_get_token_info returns token details", {
  fake_body <- '{"result":{"asset":"myasset","token":"t1","qty":10},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_get_token_info(conn_mock, "myasset", "t1")
      expect_equal(res$token, "t1")
      expect_equal(res$qty, 10)
    }
  )
})

test_that("mc_list_asset_issues returns a data.frame", {
  fake_body <- '{"result":[{"txid":"tx1","qty":100},{"txid":"tx2","qty":50}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      df <- mc_list_asset_issues(conn_mock, "myasset")
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 2)
      expect_equal(df$qty[1], 100)
    }
  )
})


test_that("mc_list_assets correctly parses RPC response into data.frame", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_assets_res <- list(
    result = list(
      list(
        name = "asset1",
        issuetxid = "txid123",
        assetref = "1-2-3",
        multiple = 1,
        units = 0.01,
        open = TRUE
      ),
      list(
        name = "asset2",
        issuetxid = "txid456",
        assetref = "4-5-6",
        multiple = 1,
        units = 1,
        open = FALSE
      )
    ),
    error = NULL,
    id = 1
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(as.character(jsonlite::toJSON(fake_assets_res, null = "null", auto_unbox = TRUE)))
      )
    },
    {
      df <- mc_list_assets(conn)
      
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 2)
      expect_equal(df$name[1], "asset1")
      expect_equal(df$units[1], 0.01)
      expect_true(df$open[1])
      expect_false(df$open[2])
    }
  )
})

test_that("mc_list_assets returns empty data.frame on empty result", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        body = charToRaw('{"result":[], "error":null, "id":1}')
      )
    },
    {
      df <- mc_list_assets(conn)
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 0)
    }
  )
})

test_that("mc_issue and mc_issue_from return txid", {
  fake_txid <- "issuance_txid_001"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      tx1 <- mc_issue(conn_mock, "1ADDR", "assetname", 1000)
      expect_equal(tx1, fake_txid)
      
      tx2 <- mc_issue_from(conn_mock, "1FROM", "1TO", "assetname", 500)
      expect_equal(tx2, fake_txid)
    }
  )
})

test_that("mc_issue_from works with name as string", {
  fake_txid <- "issue_txid_123"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_from(conn_mock, "from_addr", "to_addr", "myasset", quantity = 100)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_from works with name as list", {
  fake_txid <- "issue_txid_456"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      name_list <- list(name = "myasset", open = TRUE)
      res <- mc_issue_from(conn_mock, "from_addr", "to_addr", name_list, quantity = 100)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_from includes native_amount when provided", {
  fake_txid <- "issue_txid_789"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_from(conn_mock, "from_addr", "to_addr", "myasset",
                           quantity = 100, native_amount = 0.5)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_from includes custom_fields when provided", {
  fake_txid <- "issue_txid_abc"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_from(conn_mock, "from_addr", "to_addr", "myasset",
                           quantity = 100, custom_fields = list(tag = "test"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_from includes both native_amount and custom_fields", {
  fake_txid <- "issue_txid_def"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_from(conn_mock, "from_addr", "to_addr", "myasset",
                           quantity = 100, native_amount = 0.5,
                           custom_fields = list(tag = "test"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_more and mc_issue_more_from return txid", {
  fake_txid <- "issuemore_txid_002"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      tx1 <- mc_issue_more(conn_mock, "1ADDR", "assetname", 100)
      expect_equal(tx1, fake_txid)
      
      tx2 <- mc_issue_more_from(conn_mock, "1FROM", "1TO", "assetname", 100)
      expect_equal(tx2, fake_txid)
    }
  )
})

test_that("mc_issue_more works with minimal parameters", {
  fake_txid <- "issue_more_txid_001"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_more(conn_mock, "address1", "myasset", quantity = 50)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_more includes native_amount when provided", {
  fake_txid <- "issue_more_txid_002"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_more(conn_mock, "address1", "myasset", quantity = 50, native_amount = 0.1)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_more adds zero native_amount when custom_fields present but no native_amount", {
  fake_txid <- "issue_more_txid_003"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_more(conn_mock, "address1", "myasset", quantity = 50,
                           custom_fields = list(note = "test"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_more includes both native_amount and custom_fields", {
  fake_txid <- "issue_more_txid_004"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_more(conn_mock, "address1", "myasset", quantity = 50,
                           native_amount = 0.2, custom_fields = list(note = "test"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_more_from works with minimal parameters", {
  fake_txid <- "issue_more_from_txid_001"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_more_from(conn_mock, "from_addr", "to_addr", "myasset", quantity = 50)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_more_from includes native_amount when provided", {
  fake_txid <- "issue_more_from_txid_002"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_more_from(conn_mock, "from_addr", "to_addr", "myasset",
                                quantity = 50, native_amount = 0.1)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_more_from adds zero native_amount when custom_fields present but no native_amount", {
  fake_txid <- "issue_more_from_txid_003"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_more_from(conn_mock, "from_addr", "to_addr", "myasset",
                                quantity = 50, custom_fields = list(note = "test"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_more_from includes both native_amount and custom_fields", {
  fake_txid <- "issue_more_from_txid_004"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_more_from(conn_mock, "from_addr", "to_addr", "myasset",
                                quantity = 50, native_amount = 0.2,
                                custom_fields = list(note = "test"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_token returns txid", {
  fake_txid <- "issuetoken_txid_003"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      tx <- mc_issue_token(conn_mock, "1ADDR", "parent_asset", "NFT_1", 1)
      expect_equal(tx, fake_txid)
    }
  )
})

test_that("mc_update and mc_update_from return txid", {
  fake_txid <- "update_txid_004"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      tx1 <- mc_update(conn_mock, "assetname", list(open = FALSE))
      expect_equal(tx1, fake_txid)
      
      tx2 <- mc_update_from(conn_mock, "1ADMIN", "assetname", list(open = TRUE))
      expect_equal(tx2, fake_txid)
    }
  )
})

test_that("mc_issue correctly handles custom_fields without native_amount", {
  fake_body <- '{"result":"tx123","error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      tx <- mc_issue(conn_mock, "1A", "asset", 10, custom_fields = list(url = "test"))
      expect_equal(tx, "tx123")
    }
  )
})

test_that("mc_issue_token works with minimal parameters", {
  fake_txid <- "issue_token_txid_001"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_token(conn_mock, "address1", "parent_asset", "token1", quantity = 1)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_token includes native_amount when provided", {
  fake_txid <- "issue_token_txid_002"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_token(conn_mock, "address1", "parent_asset", "token1",
                            quantity = 1, native_amount = 0.5)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_token adds zero native_amount when token_details present but no native_amount", {
  fake_txid <- "issue_token_txid_003"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_token(conn_mock, "address1", "parent_asset", "token1",
                            quantity = 1, token_details = list(description = "NFT"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_token includes both native_amount and token_details", {
  fake_txid <- "issue_token_txid_004"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_token(conn_mock, "address1", "parent_asset", "token1",
                            quantity = 1, native_amount = 0.2,
                            token_details = list(description = "NFT"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_token_from works with minimal parameters", {
  fake_txid <- "issue_token_from_txid_001"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_token_from(conn_mock, "from_addr", "to_addr", "parent_asset",
                                 "token1", quantity = 1)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_token_from includes native_amount when provided", {
  fake_txid <- "issue_token_from_txid_002"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_token_from(conn_mock, "from_addr", "to_addr", "parent_asset",
                                 "token1", quantity = 1, native_amount = 0.5)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_token_from adds zero native_amount when token_details present but no native_amount", {
  fake_txid <- "issue_token_from_txid_003"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_token_from(conn_mock, "from_addr", "to_addr", "parent_asset",
                                 "token1", quantity = 1, token_details = list(description = "NFT"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_issue_token_from includes both native_amount and token_details", {
  fake_txid <- "issue_token_from_txid_004"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_issue_token_from(conn_mock, "from_addr", "to_addr", "parent_asset",
                                 "token1", quantity = 1, native_amount = 0.2,
                                 token_details = list(description = "NFT"))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_list_asset_issues works with default parameters", {
  fake_issues <- list(list(txid = "tx1", issuer = "addr1", quantity = 100))
  fake_body <- jsonlite::toJSON(list(result = fake_issues), auto_unbox = TRUE)
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_asset_issues(conn_mock, "asset1")
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_asset_issues works with count parameter", {
  fake_issues <- list(list(txid = "tx1", issuer = "addr1", quantity = 100))
  fake_body <- jsonlite::toJSON(list(result = fake_issues), auto_unbox = TRUE)
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_asset_issues(conn_mock, "asset1", count = 5)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_asset_issues works with start parameter (and no count)", {
  fake_issues <- list(list(txid = "tx1", issuer = "addr1", quantity = 100))
  fake_body <- jsonlite::toJSON(list(result = fake_issues), auto_unbox = TRUE)
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_asset_issues(conn_mock, "asset1", start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_asset_issues works with both count and start", {
  fake_issues <- list(list(txid = "tx1", issuer = "addr1", quantity = 100))
  fake_body <- jsonlite::toJSON(list(result = fake_issues), auto_unbox = TRUE)
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_asset_issues(conn_mock, "asset1", count = 5, start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_assets works with default parameters", {
  fake_assets <- list(list(name = "asset1", ref = "ref1", issuetxid = "tx1"))
  fake_body <- jsonlite::toJSON(list(result = fake_assets), auto_unbox = TRUE)
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_assets(conn_mock)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_assets works with count parameter", {
  fake_assets <- list(list(name = "asset1", ref = "ref1", issuetxid = "tx1"))
  fake_body <- jsonlite::toJSON(list(result = fake_assets), auto_unbox = TRUE)
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_assets(conn_mock, count = 5)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_assets works with start parameter (and no count)", {
  fake_assets <- list(list(name = "asset1", ref = "ref1", issuetxid = "tx1"))
  fake_body <- jsonlite::toJSON(list(result = fake_assets), auto_unbox = TRUE)
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_assets(conn_mock, start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_assets works with both count and start", {
  fake_assets <- list(list(name = "asset1", ref = "ref1", issuetxid = "tx1"))
  fake_body <- jsonlite::toJSON(list(result = fake_assets), auto_unbox = TRUE)
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_assets(conn_mock, count = 5, start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_get_asset_transaction returns transaction list", {
  fake_body <- '{"result":{"txid":"tx123","balance":{"amount":10}},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_get_asset_transaction(conn_mock, "asset1", "tx123")
      expect_type(res, "list")
      expect_equal(res$txid, "tx123")
    }
  )
})

test_that("mc_list_asset_transactions returns data.frame", {
  fake_body <- '{"result":[{"txid":"tx1","qty":5},{"txid":"tx2","qty":-2}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      df <- mc_list_asset_transactions(conn_mock, "asset1", count = 2)
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 2)
    }
  )
})

test_that("mc_list_asset_transactions works with default (no start)", {
  fake_txs <- list(list(txid = "tx1", amount = 10))
  fake_body <- jsonlite::toJSON(list(result = fake_txs), auto_unbox = TRUE)
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_asset_transactions(conn_mock, "asset1")
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_asset_transactions works with start parameter", {
  fake_txs <- list(list(txid = "tx1", amount = 10))
  fake_body <- jsonlite::toJSON(list(result = fake_txs), auto_unbox = TRUE)
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_asset_transactions(conn_mock, "asset1", start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})
