conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_get_new_address returns a string address", {
  fake_body <- '{"result":"1ABC1234567890","error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_get_new_address(conn_mock)
      expect_equal(res, "1ABC1234567890")
    }
  )
})

test_that("mc_get_addresses: simple mode", {
  fake_body <- '{"result":["addr1","addr2"],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_get_addresses(conn_mock, verbose = FALSE)
      expect_equal(res, c("addr1", "addr2"))
    }
  )
})

test_that("mc_get_addresses: verbose mode", {
  fake_body <- '{"result":[{"address":"addr1","ismine":true}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_get_addresses(conn_mock, verbose = TRUE)
      expect_type(res, "list")
      expect_equal(res[[1]]$address, "addr1")
    }
  )
})

test_that("mc_add_multisig_address returns address", {
  fake_body <- '{"result":"3MultiSig123","error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_add_multisig_address(conn_mock, 2, c("key1", "key2"))
      expect_equal(res, "3MultiSig123")
    }
  )
})

test_that("mc_import_address returns NULL on success", {
  fake_body <- '{"result":null,"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_import_address(conn_mock, "1Addr")
      expect_null(res)
    }
  )
})

test_that("mc_list_addresses returns data.frame", {
  fake_body <- '{"result":["addr1","addr2"],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      df <- mc_list_addresses(conn_mock, verbose = FALSE)
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 2)
      expect_equal(df$value[1], "addr1")
    }
  )
})

test_that("mc_list_addresses works with count = NULL", {
  fake_body <- jsonlite::toJSON(list(result = list()), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      result <- mc_list_addresses(conn_mock)
      expect_true(is.data.frame(result))
    }
  )
})

test_that("mc_list_addresses works with count only", {
  fake_body <- jsonlite::toJSON(list(result = list()), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      result <- mc_list_addresses(conn_mock, count = 5)
      expect_true(is.data.frame(result))
    }
  )
})

test_that("mc_list_addresses works with count and start", {
  fake_body <- jsonlite::toJSON(list(result = list()), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      result <- mc_list_addresses(conn_mock, count = 5, start = 10)
      expect_true(is.data.frame(result))
    }
  )
})

test_that("mc_create_keypairs returns a data.frame", {
  fake_body <- '{"result":[{"address":"A","pubkey":"B","privkey":"C"}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      df <- mc_create_keypairs(conn_mock, count = 1)
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 1)
      expect_equal(df$address[1], "A")
    }
  )
})

test_that("mc_create_multisig returns list with address and script", {
  fake_body <- '{"result":{"address":"multisig_addr","redeemScript":"hex_script"},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_create_multisig(conn_mock, 1, c("key1"))
      expect_type(res, "list")
      expect_equal(res$address, "multisig_addr")
      expect_equal(res$redeemScript, "hex_script")
    }
  )
})

test_that("mc_validate_address returns info", {
  fake_body <- '{"result":{"isvalid":true,"address":"1ABC"},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      res <- mc_validate_address(conn_mock, "1ABC")
      expect_true(res$isvalid)
      expect_equal(res$address, "1ABC")
    }
  )
})
