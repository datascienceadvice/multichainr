conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_list_permissions correctly parses response into data.frame", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_perms_res <- list(
    result = list(
      list(
        address = "1ABC...",
        type = "connect",
        startblock = 0,
        endblock = 1000
      ),
      list(
        address = "1ABC...",
        type = "send",
        startblock = 0,
        endblock = 4294967295
      ),
      list(
        address = "1XYZ...",
        type = "receive",
        startblock = 500,
        endblock = 4294967295
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
        body = charToRaw(jsonlite::toJSON(fake_perms_res, null = "null", auto_unbox = TRUE))
      )
    },
    {
      df <- mc_list_permissions(conn)
      
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 3)
      expect_equal(ncol(df), 4)
      expect_true("address" %in% names(df))
      expect_true("type" %in% names(df))
      
      expect_equal(df$type[1], "connect")
      expect_equal(df$endblock[1], 1000)
      expect_equal(df$address[3], "1XYZ...")
    }
  )
})

test_that("mc_list_permissions returns empty data.frame on no permissions found", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_empty <- list(result = list(), error = NULL, id = 1)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        body = charToRaw(jsonlite::toJSON(fake_empty, null = "null", auto_unbox = TRUE))
      )
    },
    {
      df <- mc_list_permissions(conn, "issue")
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 0)
    }
  )
})

test_that("mc_grant returns transaction ID on success", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_txid <- list(
    result = "789101112bcdef...",
    error = NULL,
    id = 1
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        body = charToRaw(jsonlite::toJSON(fake_txid, null = "null", auto_unbox = TRUE))
      )
    },
    {
      txid <- mc_grant(conn, "1ABC...", "send,receive")
      expect_equal(txid, "789101112bcdef...")
    }
  )
})

test_that("mc_revoke returns transaction ID on success", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_txid <- list(
    result = "1234567890abcdef...",
    error = NULL,
    id = 1
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        body = charToRaw(jsonlite::toJSON(fake_txid, null = "null", auto_unbox = TRUE))
      )
    },
    {
      txid <- mc_revoke(conn, "1ABC...", "mine")
      expect_equal(txid, "1234567890abcdef...")
    }
  )
})

test_that("mc_verify_permission returns boolean", {
  fake_body <- '{"result":true,"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_verify_permission(conn_mock, "1ABC", "send")
      expect_true(res)
    }
  )
})

test_that("mc_grant_with_data returns txid", {
  fake_txid <- "txid_123456"
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
      txid <- mc_grant_with_data(conn_mock, "1ABC", "issue", "some_metadata")
      expect_equal(txid, fake_txid)
    }
  )
})

test_that("mc_grant_from handles optional block params", {
  fake_body <- '{"result":"txid_grant_from","error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      txid <- mc_grant_from(conn_mock, "1ADMIN", "1USER", "send", start_block = 100)
      expect_equal(txid, "txid_grant_from")
    }
  )
})
