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