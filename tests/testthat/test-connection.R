test_that("mc_connect correctly create object", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  expect_s3_class(conn, "multichain_conn")
  
  expect_equal(conn$user, "u")
  expect_equal(conn$url, "http://127.0.0.1:8570")
})

test_that("mc_connect accepts a config list", {
  config <- list(host = "192.168.1.100", port = 8570, user = "myuser", password = "mypass")
  conn <- mc_connect(config)
  expect_s3_class(conn, "multichain_conn")
  expect_equal(conn$url, "http://192.168.1.100:8570")
  expect_equal(conn$user, "myuser")
  expect_equal(conn$password, "mypass")
})

test_that("mc_connect handles config list with missing host", {
  config <- list(port = 8570, user = "user", password = "pass")
  conn <- mc_connect(config)
  expect_s3_class(conn, "multichain_conn")
  expect_equal(conn$url, "http://127.0.0.1:8570")
  expect_equal(conn$user, "user")
  expect_equal(conn$password, "pass")
})

test_that("mc_connect stops when missing port", {
  expect_error(mc_connect(host = "127.0.0.1", user = "user", password = "pass"),
               "Must provide port, user, and password")
})

test_that("mc_connect stops when missing user", {
  expect_error(mc_connect(host = "127.0.0.1", port = 8570, password = "pass"),
               "Must provide port, user, and password")
})

test_that("mc_connect stops when missing password", {
  expect_error(mc_connect(host = "127.0.0.1", port = 8570, user = "user"),
               "Must provide port, user, and password")
})

test_that("print pasword hidden", {
  conn <- mc_connect(port = 8570, user = "u", password = "secret_password")
  output <- capture.output(print(conn))
  
  expect_true(any(grepl("HIDDEN", output)))
  expect_false(any(grepl("secret_password", output)))
})