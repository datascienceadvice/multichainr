conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_get_peer_info returns data.frame of peers", {
  fake_body <- '{"result":[{"addr":"127.0.0.1:8571","pingtime":0.001,"version":20012}],"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, headers = list("Content-Type" = "application/json"), body = charToRaw(fake_body))
    },
    {
      df <- mc_get_peer_info(conn_mock)
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 1)
      expect_equal(df$addr[1], "127.0.0.1:8571")
    }
  )
})

test_that("mc_get_network_info returns list", {
  fake_body <- '{"result":{"version":20300,"locals":[{"address":"192.168.1.5"}]},"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_get_network_info(conn_mock)
      expect_type(res, "list")
      expect_equal(res$locals[[1]]$address, "192.168.1.5")
    }
  )
})

test_that("mc_ping returns NULL", {
  fake_body <- '{"result":null,"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      res <- mc_ping(conn_mock)
      expect_null(res)
    }
  )
})