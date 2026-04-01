test_that("mc_connect correctly create object", {
  conn <- mc_connect(port = 8570, user = "u", password = "p")
  
  expect_s3_class(conn, "multichain_conn")
  
  expect_equal(conn$user, "u")
  expect_equal(conn$url, "http://127.0.0.1:8570")
})

test_that("print pasword hidden", {
  conn <- mc_connect(port = 8570, user = "u", password = "secret_password")
  output <- capture.output(print(conn))
  
  expect_true(any(grepl("HIDDEN", output)))
  expect_false(any(grepl("secret_password", output)))
})
