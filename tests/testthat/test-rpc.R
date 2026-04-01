test_that("mc_rpc handles successful response correctly", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_result <- list(
    result = list(chain_name = "testchain", version = "2.0"),
    error = NULL,
    id = 1
  )
  fake_body_raw <- charToRaw(as.character(jsonlite::toJSON(fake_result, null = "null", auto_unbox = TRUE)))
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = fake_body_raw
      )
    },
    {
      res <- mc_rpc(conn, "getinfo")
      
      expect_equal(res$chain_name, "testchain")
      expect_equal(res$version, "2.0")
    }
  )
})

test_that("mc_rpc throws error on 401 Unauthorized", {
  conn <- mc_connect(port = 8570, user = "u", password = "wrong")
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 401),
    {
      expect_error(mc_rpc(conn, "getinfo"), "401 Unauthorized")
    }
  )
})

test_that("mc_rpc handles empty body response", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw("")),
    {
      expect_error(mc_rpc(conn, "getinfo"), "Empty or invalid response")
    }
  )
})
