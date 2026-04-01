test_that("%||% works correctly", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(FALSE %||% TRUE, FALSE)
  expect_equal(numeric(0) %||% 1, numeric(0))
})

test_that("hex_to_char decodes strings correctly", {
  expect_equal(hex_to_char("48656c6c6f"), "Hello")
  expect_equal(hex_to_char("74657374"), "test")
  expect_equal(hex_to_char(""), "")
  expect_equal(hex_to_char(NULL), "")
})

test_that("hex_to_char handles wrong input", {
  expect_equal(hex_to_char("abc"), "abc")
  
  expect_equal(hex_to_char("zztop!"), "zztop!")
})

test_that("rpc_res_to_df transforms list to data.frame", {
  input <- list(
    list(id = 1, name = "first"),
    list(id = 2, name = "second")
  )
  
  df <- rpc_res_to_df(input)
  
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
  expect_equal(df$name[1], "first")
})

test_that("rpc_res_to_df handles NULL correctly", {
  input <- list(
    list(id = 1, value = NULL),
    list(id = 2, value = 100)
  )
  
  df <- rpc_res_to_df(input)
  
  expect_true(is.na(df$value[1]))
  expect_equal(df$value[2], 100)
})

test_that("rpc_res_to_df returns empty df from empty input", {
  expect_equal(nrow(rpc_res_to_df(list())), 0)
  expect_equal(nrow(rpc_res_to_df(NULL)), 0)
})

test_that("rpc_res_to_df handles mismatched columns", {
  res <- list(
    list(a = 1, b = 2),
    list(a = 3, c = 4) # здесь 'c' вместо 'b'
  )
  df <- rpc_res_to_df(res)
  expect_equal(ncol(df), 3) # a, b, c
  expect_true(is.na(df$c[1]))
  expect_true(is.na(df$b[2]))
})

test_that("rpc_res_to_df handles simple lists of strings", {
  simple_list <- list("address1", "address2", "address3")
  df <- rpc_res_to_df(simple_list)
  
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3)
  expect_equal(ncol(df), 1)
  expect_equal(df$value[1], "address1")
})

test_that("mc_rpc throw error on wrong object", {
  expect_error(mc_rpc(list(), "getinfo"), "must be a 'multichain_conn' object")
})

test_that("mc_rpc handles rpc node error correctly", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  fake_error_res <- list(
    result = NULL,
    error = list(code = -32601, message = "Method not found"),
    id = 1
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(fake_error_res, auto_unbox = TRUE))
      )
    },
    {
      expect_error(mc_rpc(conn, "unknown_method"), "Method not found")
    }
  )
})

test_that("mc_rpc: success and scipen restoration", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  original_scipen <- getOption("scipen")
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw('{"result":"ok","error":null,"id":1}')
      )
    },
    {
      res <- mc_rpc(conn, "getinfo")
      expect_equal(res, "ok")
    }
  )
  
  expect_equal(getOption("scipen"), original_scipen)
})

test_that("mc_rpc: handles RPC error", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw('{"result":null,"error":{"code":-1,"message":"test_fail"},"id":1}')
      )
    },
    {
      expect_error(mc_rpc(conn, "getinfo"), "test_fail")
    }
  )
})

test_that("mc_rpc handles timeouts", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  httr2::with_mocked_responses(
    function(req) {
      stop("Could not resolve host")
    },
    {
      expect_error(mc_rpc(conn, "getinfo"), "Failed to connect to MultiChain node")
    }
  )
})
