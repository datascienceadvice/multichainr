test_that("mc_get_blockchain_params returns list of parameters", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_params <- list(
    result = list(
      "protocol-version" = 20011,
      "chain-description" = "MultiChain test_chain",
      "root-stream-name" = "root",
      "target-block-time" = 15,
      "maximum-block-size" = 8388608
    ),
    error = NULL,
    id = 1
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(as.character(jsonlite::toJSON(fake_params, null = "null", auto_unbox = TRUE)))
      )
    },
    {
      params <- mc_get_blockchain_params(conn)
      
      expect_type(params, "list")
      expect_equal(params[["protocol-version"]], 20011)
      expect_equal(params[["target-block-time"]], 15)
      expect_equal(params[["root-stream-name"]], "root")
    }
  )
})

test_that("mc_get_runtime_params returns node runtime settings", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_runtime_params <- list(
    result = list(
      "port" = 8571,
      "miningrequirespeers" = TRUE,
      "minecombine" = 0.3,
      "lockadminminer" = FALSE,
      "maxconnections" = 125,
      "autosubscribe" = "assets,streams"
    ),
    error = NULL,
    id = 1
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(as.character(jsonlite::toJSON(fake_runtime_params, null = "null", auto_unbox = TRUE)))
      )
    },
    {
      params <- mc_get_runtime_params(conn)
      
      expect_type(params, "list")
      expect_equal(params$port, 8571)
      expect_equal(params$maxconnections, 125)
      expect_true(params$miningrequirespeers)
      expect_equal(params$autosubscribe, "assets,streams")
    }
  )
})

test_that("mc_set_runtime_param correctly handles success response", {
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
        body = charToRaw(jsonlite::toJSON(fake_response, null = "null", auto_unbox = TRUE))
      )
    },
    {
      res <- mc_set_runtime_param(conn, "autosubscribe", FALSE)
      expect_null(res)
    }
  )
})

test_that("mc_set_runtime_param throws error for invalid parameter name (R side validation)", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")

  expect_error(
    mc_set_runtime_param(conn, "invalid_param_name", 123),
    "'arg' should be one of"
  )
})

test_that("mc_set_runtime_param throws error for invalid connection object", {
  conn <- list(url = "not_a_proper_conn")
  
  expect_error(
    mc_set_runtime_param(conn, "autosubscribe", TRUE),
    "must be a 'multichain_conn' object"
  )
})

test_that("mc_set_runtime_param handles RPC error correctly", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_error <- list(
    result = NULL,
    error = list(code = -1, message = "RPC error occurred"),
    id = 1
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(fake_error, null = "null", auto_unbox = TRUE))
      )
    },
    {
      expect_error(
        mc_set_runtime_param(conn, "lockblock", TRUE),
        "RPC error occurred"
      )
    }
  )
})










test_that("mc_get_info returns a list with expected node information", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_info <- list(
    result = list(
      version = "2.3.3",
      protocolversion = 20012,
      walletversion = 60000,
      balance = 150.5,
      blocks = 1234,
      connections = 8,
      proxy = "",
      difficulty = 1.0,
      testnet = FALSE,
      errors = ""
    ),
    error = NULL,
    id = 1
  )
  
  # 2. Convert to RAW for the mock
  fake_body_raw <- charToRaw(as.character(
    jsonlite::toJSON(fake_info, null = "null", auto_unbox = TRUE)
  ))
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = fake_body_raw
      )
    },
    {
      # 3. Call the function
      info <- mc_get_info(conn)
      
      # 4. Expectations
      expect_type(info, "list")
      expect_equal(info$version, "2.3.3")
      expect_equal(info$blocks, 1234)
      expect_equal(info$balance, 150.5)
      expect_false(info$testnet)
      
      # Verify some standard MultiChain keys exist
      expect_true("protocolversion" %in% names(info))
      expect_true("difficulty" %in% names(info))
    }
  )
})

test_that("mc_get_info handles connection or auth errors via mc_rpc", {
  conn <- mc_connect(port = 8570, user = "u", password = "wrong_password")
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 401)
    },
    {
      expect_error(mc_get_info(conn), "401 Unauthorized")
    }
  )
})

test_that("mc_get_init_status returns progress during loading", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_status <- list(
    result = list(
      status = "loading blocks",
      progress = 0.45
    ),
    error = NULL,
    id = 1
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(as.character(jsonlite::toJSON(fake_status, null = "null", auto_unbox = TRUE)))
      )
    },
    {
      res <- mc_get_init_status(conn)
      
      expect_type(res, "list")
      expect_equal(res$status, "loading blocks")
      expect_equal(res$progress, 0.45)
    }
  )
})

test_that("mc_get_init_status returns ready status", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_ready <- list(
    result = list(
      status = "ready"
    ),
    error = NULL,
    id = 1
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(as.character(jsonlite::toJSON(fake_ready, null = "null", auto_unbox = TRUE)))
      )
    },
    {
      res <- mc_get_init_status(conn)
      expect_equal(res$status, "ready")
    }
  )
})

test_that("mc_help returns a string of available commands", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_help_all <- list(
    result = "== Blockchain ==\ngetblockchaininfo\ngetblockhash\n...",
    error = NULL,
    id = 1
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(as.character(jsonlite::toJSON(fake_help_all, null = "null", auto_unbox = TRUE)))
      )
    },
    {
      res <- mc_help(conn)
      
      expect_type(res, "character")
      expect_true(grepl("Blockchain", res))
    }
  )
})

test_that("mc_help returns detailed help for a specific command", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_help_getinfo <- list(
    result = "getinfo\nReturns an object containing various state info.",
    error = NULL,
    id = 1
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(as.character(jsonlite::toJSON(fake_help_getinfo, null = "null", auto_unbox = TRUE)))
      )
    },
    {
      res <- mc_help(conn, "getinfo")
      
      expect_type(res, "character")
      expect_true(grepl("getinfo", res))
      expect_true(grepl("Returns an object", res))
    }
  )
})
