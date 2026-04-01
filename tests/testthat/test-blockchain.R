conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

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