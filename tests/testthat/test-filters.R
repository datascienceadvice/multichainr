conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_create_stream_filter calls create with streamfilter", {
  fake_txid <- "streamfilter_txid"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_stream_filter(conn_mock, "myfilter", list(), "js_code")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_create_tx_filter calls create with txfilter", {
  fake_txid <- "txfilter_txid"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_tx_filter(conn_mock, "myfilter", list(), "js_code")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_create_upgrade calls create with upgrade", {
  fake_txid <- "upgrade_txid"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_upgrade(conn_mock, "myupgrade", list("target-block-time" = 20))
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_approve_from calls approvefrom", {
  fake_txid <- "approve_txid"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_approve_from(conn_mock, "from_addr", "entity", TRUE)
      expect_equal(res, fake_txid)
    }
  )
})

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

test_that("mc_list_stream_filters returns data frame", {
  fake_filters <- list(list(name = "f1", version = 1))
  fake_body <- jsonlite::toJSON(list(result = fake_filters), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stream_filters(conn_mock)
      expect_s3_class(res, "data.frame")
      expect_equal(res$name, "f1")
    }
  )
})

test_that("mc_list_tx_filters returns data frame", {
  fake_filters <- list(list(name = "f2", version = 2))
  fake_body <- jsonlite::toJSON(list(result = fake_filters), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_tx_filters(conn_mock)
      expect_s3_class(res, "data.frame")
      expect_equal(res$name, "f2")
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

test_that("mc_test_tx_filter works without tx", {
  fake_result <- "test result"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_test_tx_filter(conn_mock, list(), "js_code")
      expect_equal(res, fake_result)
    }
  )
})

test_that("mc_test_tx_filter works with tx", {
  fake_result <- "test result with tx"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_test_tx_filter(conn_mock, list(), "js_code", tx = "abc")
      expect_equal(res, fake_result)
    }
  )
})

test_that("mc_run_tx_filter calls runtxfilter", {
  fake_result <- "run result"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_run_tx_filter(conn_mock, "myfilter", "txid")
      expect_equal(res, fake_result)
    }
  )
})

test_that("mc_test_stream_filter works without tx", {
  fake_result <- "stream test"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_test_stream_filter(conn_mock, list(), "js_code")
      expect_equal(res, fake_result)
    }
  )
})

test_that("mc_test_stream_filter works with tx only", {
  fake_result <- "stream test with tx"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_test_stream_filter(conn_mock, list(), "js_code", tx = "abc")
      expect_equal(res, fake_result)
    }
  )
})

test_that("mc_test_stream_filter works with tx and vout", {
  fake_result <- "stream test with vout"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_test_stream_filter(conn_mock, list(), "js_code", tx = "abc", vout = 1)
      expect_equal(res, fake_result)
    }
  )
})

test_that("mc_run_stream_filter works without vout", {
  fake_result <- "run stream"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_run_stream_filter(conn_mock, "myfilter", "txid")
      expect_equal(res, fake_result)
    }
  )
})

test_that("mc_run_stream_filter works with vout", {
  fake_result <- "run stream with vout"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_run_stream_filter(conn_mock, "myfilter", "txid", vout = 2)
      expect_equal(res, fake_result)
    }
  )
})

test_that("mc_create_library works with default updatemode", {
  fake_txid <- "lib_txid"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_library(conn_mock, "mylib", js_code = "code")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_create_library works with explicit updatemode", {
  fake_txid <- "lib_txid2"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_library(conn_mock, "mylib", updatemode = "approve", js_code = "code")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_add_library_update calls addlibraryupdate", {
  fake_txid <- "update_txid"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_add_library_update(conn_mock, "mylib", "v2", "new code")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_add_library_update_from calls addlibraryupdatefrom", {
  fake_txid <- "updatefrom_txid"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_add_library_update_from(conn_mock, "from_addr", "mylib", "v2", "new code")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_get_library_code works without updatename", {
  fake_code <- "library code"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_code)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_library_code(conn_mock, "mylib")
      expect_equal(res, fake_code)
    }
  )
})

test_that("mc_get_library_code works with updatename", {
  fake_code <- "update code"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_code)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_library_code(conn_mock, "mylib", updatename = "v2")
      expect_equal(res, fake_code)
    }
  )
})

test_that("mc_test_library works with no arguments", {
  fake_result <- "test result"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_test_library(conn_mock)
      expect_equal(res, fake_result)
    }
  )
})

test_that("mc_test_library works with library only", {
  fake_result <- "test lib"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_test_library(conn_mock, library = "mylib")
      expect_equal(res, fake_result)
    }
  )
})

test_that("mc_test_library works with library and updatename", {
  fake_result <- "test update"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_test_library(conn_mock, library = "mylib", updatename = "v2")
      expect_equal(res, fake_result)
    }
  )
})

test_that("mc_test_library works with library, updatename and js_code", {
  fake_result <- "test with js"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_result)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_test_library(conn_mock, library = "mylib", updatename = "v2", js_code = "code")
      expect_equal(res, fake_result)
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

test_that("mc_create_variable calls create with variable", {
  fake_txid <- "var_txid"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_variable(conn_mock, "myvar")
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_create_variable_from calls createfrom", {
  fake_txid <- "varfrom_txid"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_create_variable_from(conn_mock, "from_addr", "myvar")
      expect_equal(res, fake_txid)
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

test_that("mc_set_variable_value_from calls setvariablevaluefrom", {
  fake_txid <- "setvar_txid"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_set_variable_value_from(conn_mock, "from_addr", "myvar", value = 42)
      expect_equal(res, fake_txid)
    }
  )
})

test_that("mc_get_variable_info calls getvariableinfo", {
  fake_info <- list(name = "myvar", open = TRUE)
  fake_body <- jsonlite::toJSON(list(result = fake_info), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_variable_info(conn_mock, "myvar", verbose = TRUE)
      expect_equal(res$name, "myvar")
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


test_that("mc_list_variables works with default parameters", {
  fake_vars <- list(list(name = "v1", ref = "ref1"))
  fake_body <- jsonlite::toJSON(list(result = fake_vars), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_variables(conn_mock)
      expect_s3_class(res, "data.frame")
      expect_equal(res$name, "v1")
    }
  )
})

test_that("mc_list_variables works with count parameter", {
  fake_vars <- list(list(name = "v1", ref = "ref1"))
  fake_body <- jsonlite::toJSON(list(result = fake_vars), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_variables(conn_mock, count = 5)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_variables works with start parameter and no count", {
  fake_vars <- list(list(name = "v1", ref = "ref1"))
  fake_body <- jsonlite::toJSON(list(result = fake_vars), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_variables(conn_mock, start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})

test_that("mc_list_variables works with both count and start", {
  fake_vars <- list(list(name = "v1", ref = "ref1"))
  fake_body <- jsonlite::toJSON(list(result = fake_vars), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_variables(conn_mock, count = 5, start = 10)
      expect_s3_class(res, "data.frame")
    }
  )
})


