test_that("hex_to_char correctly decodes known values", {
  expect_equal(hex_to_char("48656c6c6f"), "Hello")
  expect_equal(hex_to_char("7b7d"), "{}")
  expect_equal(hex_to_char(""), "")
})
