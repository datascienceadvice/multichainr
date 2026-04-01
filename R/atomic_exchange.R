#' Add to a raw atomic exchange transaction
#' 
#' @param conn Connection object.
#' @param tx_hex Hex string of the partial exchange transaction.
#' @param txid Transaction ID of the output being added to the offer.
#' @param vout Vout of the output being added.
#' @param amounts Assets/currency asked for in exchange for this addition.
#' @return A list containing 'hex' (new partial transaction) and 'complete' (logical).
#' @export
mc_append_raw_exchange <- function(conn, tx_hex, txid, vout, amounts) {
  mc_rpc(conn, "appendrawexchange", list(tx_hex, txid, as.integer(vout), amounts))
}

#' Finalize an atomic exchange transaction
#' 
#' @param conn Connection object.
#' @param tx_hex Hex string of the partial exchange transaction.
#' @param txid Transaction ID of the completing output.
#' @param vout Vout of the completing output.
#' @param amounts Assets/currency for the final part of the exchange.
#' @param data Optional metadata (string or list) to include.
#' @return A raw transaction hex ready for sendrawtransaction.
#' @export
mc_complete_raw_exchange <- function(conn, tx_hex, txid, vout, amounts, data = NULL) {
  params <- list(tx_hex, txid, as.integer(vout), amounts)
  
  if (!is.null(data)) {
    if (is.list(data)) data <- jsonlite::toJSON(data, auto_unbox = TRUE)
    data_hex <- paste(charToRaw(as.character(data)), collapse = "")
    params <- c(params, list(data_hex))
  }
  
  mc_rpc(conn, "completerawexchange", params)
}

#' Create a new atomic exchange transaction
#' 
#' @param conn Connection object.
#' @param txid Transaction ID from preparelockunspent.
#' @param vout Vout from preparelockunspent.
#' @param amounts Assets/currency asked for in exchange.
#' @return A raw partial transaction in hexadecimal.
#' @export
mc_create_raw_exchange <- function(conn, txid, vout, amounts) {
  mc_rpc(conn, "createrawexchange", list(txid, as.integer(vout), amounts))
}



#' Decode a raw exchange transaction
#' 
#' @param conn Connection object.
#' @param tx_hex Hex string of the exchange transaction.
#' @param verbose If TRUE, lists all individual stages in the exchange.
#' @export
mc_decode_raw_exchange <- function(conn, tx_hex, verbose = FALSE) {
  mc_rpc(conn, "decoderawexchange", list(tx_hex, verbose))
}

#' Disable an offer of exchange
#' 
#' @param conn Connection object.
#' @param tx_hex Hex string of the exchange transaction.
#' @return Transaction ID of the disabling transaction.
#' @export
mc_disable_raw_transaction <- function(conn, tx_hex) {
  mc_rpc(conn, "disablerawtransaction", list(tx_hex))
}

#' Prepare an unspent transaction output for exchange
#' 
#' @param conn Connection object.
#' @param amounts List of assets or numeric amount of native currency.
#' @param lock If TRUE (default), the output is locked for automatic spending.
#' @return A list containing 'txid' and 'vout'.
#' @export
mc_prepare_lock_unspent <- function(conn, amounts, lock = TRUE) {
  mc_rpc(conn, "preparelockunspent", list(amounts, lock))
}

#' Prepare an unspent output from a specific address
#' 
#' @param conn Connection object.
#' @param from_address Sender address.
#' @param amounts List of assets or numeric amount.
#' @param lock If TRUE (default), the output is locked.
#' @export
mc_prepare_lock_unspent_from <- function(conn, from_address, amounts, lock = TRUE) {
  mc_rpc(conn, "preparelockunspentfrom", list(from_address, amounts, lock))
}

