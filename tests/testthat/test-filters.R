conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_get_filter_code returns string", {
  fake_code <- "function filtertx() { return 'ok'; }"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_code)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200, 
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_get_filter_code(conn_mock, "myfilter")
      expect_equal(res, fake_code)
    }
  )
})

test_that("mc_list_upgrades returns data.frame", {
  fake_body <- '{"result":[{"name":"up1","appliedblock":100,"approved":true}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200, 
        headers = list("Content-Type" = "application/json"), 
        body = charToRaw(fake_body)
      )
    },
    {
      df <- mc_list_upgrades(conn_mock)
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 1)
      expect_equal(df$name[1], "up1")
    }
  )
})

test_that("mc_test_tx_filter returns compilation status", {
  fake_body <- '{"result":{"compiled":true,"passed":true,"log":[]},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200, 
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_test_tx_filter(conn_mock, list(), "function filtertx(){}")
      expect_type(res, "list")
      expect_true(res$compiled)
    }
  )
})

test_that("mc_get_variable_value returns JSON value", {
  fake_body <- '{"result":{"temp":25,"status":"ok"},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200, 
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_get_variable_value(conn_mock, "temp_var")
      
      expect_type(res, "list")
      expect_equal(res$temp, 25)
      expect_equal(res$status, "ok")
    }
  )
})

test_that("mc_get_variable_history returns data.frame", {
  fake_body <- '{"result":[
    {"value":10,"blocktime":1600000000},
    {"value":20,"blocktime":1600000100}
  ],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200, 
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      df <- mc_get_variable_history(conn_mock, "my_var")
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 2)
      expect_equal(df$value[2], 20)
    }
  )
})

test_that("mc_list_libraries returns data.frame", {
  fake_body <- '{"result":[{"name":"lib1","updatemode":"instant"}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      df <- mc_list_libraries(conn_mock)
      expect_s3_class(df, "data.frame")
      expect_equal(df$name[1], "lib1")
    }
  )
})

test_that("mc_set_variable_value returns txid", {
  fake_txid <- "tx_var_set_123"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      tx <- mc_set_variable_value(conn_mock, "my_var", list(a = 1))
      expect_equal(tx, fake_txid)
    }
  )
})
