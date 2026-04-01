conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_sign_message returns a base64 string", {
  fake_signature <- "H6S/P0D..."
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_signature)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200, 
        headers = list("Content-Type" = "application/json"), 
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_sign_message(conn_mock, "1ADDR", "Hello MultiChain")
      expect_type(res, "character")
      expect_equal(res, fake_signature)
    }
  )
})

test_that("mc_verify_message returns logical TRUE/FALSE", {
  fake_body_true <- '{"result":true,"error":null,"id":1}'
  fake_body_false <- '{"result":false,"error":null,"id":1}'
  
  # Тест успешной проверки
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body_true))
    },
    {
      res <- mc_verify_message(conn_mock, "1ADDR", "sig", "msg")
      expect_true(res)
    }
  )
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body_false))
    },
    {
      res <- mc_verify_message(conn_mock, "1ADDR", "wrong_sig", "msg")
      expect_false(res)
    }
  )
})
