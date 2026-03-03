test_that("mc_export_private_key returns the key string", {
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
      key <- mc_export_private_key(conn, "1ABC...")
      expect_equal(key, "Vp4k8S...dummy_wif_key...")
    }
  )
})

test_that("mc_import_private_key handles null successful response", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  # Successful importprivkey returns result: null
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
      res <- mc_import_private_key(conn, "Vp4k8S...", label = "imported", rescan = FALSE)
      expect_null(res)
    }
  )
})
