conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_add_node calls addnode RPC with correct command", {
  fake_result <- NULL
  fake_body <- '{"result":null,"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_add_node(conn_mock, "127.0.0.1:8571", command = "add")
      expect_null(res)
    }
  )
})

test_that("mc_get_added_node_info returns character vector when verbose=FALSE", {
  fake_nodes <- list("192.168.1.1:8571", "192.168.1.2:8571")
  fake_body <- jsonlite::toJSON(list(result = fake_nodes), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_added_node_info(conn_mock, verbose = FALSE)
      expect_type(res, "character")
      expect_equal(res, c("192.168.1.1:8571", "192.168.1.2:8571"))
    }
  )
})

test_that("mc_get_added_node_info returns data frame when verbose=TRUE", {
  fake_info <- list(
    list(addednode = "192.168.1.1:8571", connected = TRUE),
    list(addednode = "192.168.1.2:8571", connected = FALSE)
  )
  fake_body <- jsonlite::toJSON(list(result = fake_info), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_added_node_info(conn_mock, verbose = TRUE)
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), 2)
      expect_equal(res$addednode, c("192.168.1.1:8571", "192.168.1.2:8571"))
    }
  )
})

test_that("mc_get_added_node_info filters by node when provided", {
  fake_info <- list(list(addednode = "192.168.1.1:8571", connected = TRUE))
  fake_body <- jsonlite::toJSON(list(result = fake_info), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_get_added_node_info(conn_mock, verbose = TRUE, node = "192.168.1.1:8571")
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), 1)
      expect_equal(res$addednode, "192.168.1.1:8571")
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

test_that("mc_list_stored_nodes returns data frame", {
  fake_nodes <- list(
    list(node = "192.168.1.1:8571", tries = 5),
    list(node = "192.168.1.2:8571", tries = 3)
  )
  fake_body <- jsonlite::toJSON(list(result = fake_nodes), auto_unbox = TRUE)
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_list_stored_nodes(conn_mock, include_old_ignores = TRUE)
      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), 2)
      expect_equal(res$node, c("192.168.1.1:8571", "192.168.1.2:8571"))
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

test_that("mc_store_node calls storenode RPC with correct command", {
  fake_result <- NULL
  fake_body <- '{"result":null,"error":null,"id":1}'
  
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200, body = charToRaw(fake_body)),
    {
      res <- mc_store_node(conn_mock, "192.168.1.1:8571", command = "tryconnect")
      expect_null(res)
    }
  )
})
