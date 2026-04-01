conn_mock <- mc_connect(port = 8570, user = "u", password = "p")

test_that("mc_send and mc_send_from return txid", {
  fake_txid <- "tx_send_123"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      # Простая отправка нативной валюты
      tx1 <- mc_send(conn_mock, "1ADDR", 10.5)
      expect_equal(tx1, fake_txid)
      
      # Отправка с конкретного адреса
      tx2 <- mc_send_from(conn_mock, "1FROM", "1TO", list(asset1 = 100))
      expect_equal(tx2, fake_txid)
    }
  )
})

test_that("mc_send_asset and mc_send_asset_from work correctly", {
  fake_txid <- "tx_asset_456"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(status_code = 200, body = charToRaw(fake_body))
    },
    {
      tx1 <- mc_send_asset(conn_mock, "1ADDR", "assetA", 50)
      expect_equal(tx1, fake_txid)
      
      tx2 <- mc_send_asset_from(conn_mock, "1FROM", "1TO", "assetA", 50)
      expect_equal(tx2, fake_txid)
    }
  )
})

test_that("mc_send_with_data returns txid and handles metadata", {
  fake_txid <- "tx_data_789"
  fake_body <- sprintf('{"result":"%s","error":null,"id":1}', fake_txid)
  
  httr2::with_mocked_responses(
    function(req) {
      # Просто возвращаем успешный ответ. 
      # Если бы hex-конвертация внутри mc_send_with_data сломалась, 
      # мы бы даже не дошли до этого мока.
      httr2::response(
        status_code = 200, 
        headers = list("Content-Type" = "application/json"),
        body = charToRaw(fake_body)
      )
    },
    {
      # Проверяем передачу строки
      tx1 <- mc_send_with_data(conn_mock, "1ADDR", 0, "Hello")
      expect_equal(tx1, fake_txid)
      
      # Проверяем передачу списка (JSON)
      tx2 <- mc_send_with_data(conn_mock, "1ADDR", 0, list(key = "val"))
      expect_equal(tx2, fake_txid)
    }
  )
})
