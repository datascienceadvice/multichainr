conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

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
      # Проверка с вектором задач
      res1 <- mc_pause(conn_mock, c("mining", "incoming"))
      expect_null(res1)
      
      # Проверка с одной задачей
      res2 <- mc_resume(conn_mock, "offchain")
      expect_null(res2)
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