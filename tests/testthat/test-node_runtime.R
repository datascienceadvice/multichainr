conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_clear_mempool calls clearmempool RPC", {
  fake_result <- "Mempool cleared"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_clear_mempool(conn_mock)
      expect_equal(res, fake_result)
    }
  )
})

test_that("mc_pause and mc_resume return NULL on success", {
  fake_body <- '{"result":null,"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200, 
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res1 <- mc_pause(conn_mock, c("mining", "incoming"))
      expect_null(res1)
      
      res2 <- mc_resume(conn_mock, "offchain")
      expect_null(res2)
    }
  )
})

test_that("mc_resume handles a single task string", {
  fake_result <- "Resumed"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_resume(conn_mock, "mining")
      expect_equal(res, fake_result)
    }
  )
})

test_that("mc_resume handles multiple tasks as a vector", {
  fake_result <- "Resumed"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_resume(conn_mock, c("mining", "incoming"))
      expect_equal(res, fake_result)
    }
  )
})

test_that("mc_get_chunk_queue_info returns list", {
  fake_body <- '{"result":{"waiting":{"chunks":5,"bytes":1024},"querying":{"chunks":0}},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_get_chunk_queue_info(conn_mock)
      expect_type(res, "list")
      expect_equal(res$waiting$chunks, 5)
    }
  )
})

test_that("mc_get_chunk_queue_totals calls getchunkqueuetotals RPC", {
  fake_totals <- list(delivered = 100, undelivered = 5, timeouts = 0)
  fake_body <- jsonlite::toJSON(list(result = fake_totals), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_chunk_queue_totals(conn_mock)
      expect_equal(res$delivered, 100)
      expect_equal(res$undelivered, 5)
    }
  )
})

test_that("mc_set_last_block returns new tip hash", {
  fake_hash <- "0000abc123..."
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_hash)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_set_last_block(conn_mock, 1000)
      expect_equal(res, fake_hash)
    }
  )
})
