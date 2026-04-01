#' Create a raw transaction
#' 
#' @param conn Connection object.
#' @param inputs List of lists, each containing "txid" and "vout".
#' @param outputs List/object mapping addresses to amounts.
#' @param data Optional array of metadata (strings or lists).
#' @param action Optional action: "lock", "sign", "lock,sign", or "send".
#' @export
mc_create_raw_transaction <- function(conn, inputs, outputs, data = list(), action = "") {
  # Принудительно vout к integer в каждом входе
  inputs <- lapply(inputs, function(x) {
    x$vout <- as.integer(x$vout)
    x
  })
  
  # Обработка метаданных в HEX
  if (length(data) > 0) {
    data <- lapply(data, function(d) {
      if (is.list(d)) d <- jsonlite::toJSON(d, auto_unbox = TRUE)
      paste(charToRaw(as.character(d)), collapse = "")
    })
  }
  
  mc_rpc(conn, "createrawtransaction", list(inputs, outputs, data, action))
}

#' Create and fund a raw transaction from a specific address
#' 
#' @param conn Connection object.
#' @param from_address Sender address.
#' @param to_amounts List mapping recipient addresses to amounts.
#' @param data Optional metadata array.
#' @param action Optional action (e.g. "send", "sign").
#' @export
mc_create_raw_send_from <- function(conn, from_address, to_amounts, data = list(), action = "") {
  if (length(data) > 0) {
    data <- lapply(data, function(d) {
      if (is.list(d)) d <- jsonlite::toJSON(d, auto_unbox = TRUE)
      paste(charToRaw(as.character(d)), collapse = "")
    })
  }
  mc_rpc(conn, "createrawsendfrom", list(from_address, to_amounts, data, action))
}

#' Add a change output to a raw transaction
#' 
#' @param conn Connection object.
#' @param tx_hex Hex string of the raw transaction.
#' @param address Address to receive the change.
#' @param native_fee Optional native fee amount.
#' @export
mc_append_raw_change <- function(conn, tx_hex, address, native_fee = NULL) {
  params <- list(tx_hex, address)
  if (!is.null(native_fee)) params <- c(params, list(as.numeric(native_fee)))
  mc_rpc(conn, "appendrawchange", params)
}

#' Add metadata to a raw transaction
#' 
#' @param conn Connection object.
#' @param tx_hex Raw transaction hex.
#' @param data Data string or list.
#' @export
mc_append_raw_data <- function(conn, tx_hex, data) {
  if (is.list(data)) data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  data_hex <- paste(charToRaw(as.character(data)), collapse = "")
  mc_rpc(conn, "appendrawdata", list(tx_hex, data_hex))
}

#' Add inputs and outputs to a raw transaction
#' @export
mc_append_raw_transaction <- function(conn, tx_hex, inputs = list(), outputs = list()) {
  inputs <- lapply(inputs, function(x) {
    x$vout <- as.integer(x$vout)
    x
  })
  mc_rpc(conn, "appendrawtransaction", list(tx_hex, inputs, outputs))
}

#' Decode a raw transaction hex
#' @export
mc_decode_raw_transaction <- function(conn, tx_hex) {
  mc_rpc(conn, "decoderawtransaction", list(tx_hex))
}

#' Sign a raw transaction
#' 
#' @param conn Connection object.
#' @param tx_hex Raw transaction hex.
#' @param parents Optional list of parent outputs for signing.
#' @param private_keys Optional vector of private keys (WIF).
#' @param sighashtype Signature hash type (default "ALL").
#' @return A list with "hex" and "complete" (logical).
#' @export
mc_sign_raw_transaction <- function(conn, tx_hex, parents = NULL, private_keys = NULL, sighashtype = "ALL") {
  params <- list(tx_hex, parents, private_keys, sighashtype)
  mc_rpc(conn, "signrawtransaction", params)
}

#' Send a signed raw transaction to the network
#' @return Transaction ID.
#' @export
mc_send_raw_transaction <- function(conn, tx_hex) {
  mc_rpc(conn, "sendrawtransaction", list(tx_hex))
}