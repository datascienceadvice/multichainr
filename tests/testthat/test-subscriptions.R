conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_subscribe returns NULL on success", {
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
      res1 <- mc_subscribe(conn_mock, "stream1")
      expect_null(res1)
      
      res2 <- mc_subscribe(conn_mock, c("asset1", "stream2"), rescan = FALSE)
      expect_null(res2)
    }
  )
})

test_that("mc_unsubscribe returns NULL on success", {
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
      res <- mc_unsubscribe(conn_mock, "stream1", purge = TRUE)
      expect_null(res)
    }
  )
})
