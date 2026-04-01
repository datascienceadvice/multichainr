#' Add to a raw atomic exchange transaction
#'
#' Appends a new input–output pair to a partially constructed atomic exchange
#' transaction. This function is used when multiple parties are contributing
#' to the exchange, each adding their own locked output and specifying what
#' they want in return.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param tx_hex Character string. Hexadecimal representation of the partial
#'   exchange transaction.
#' @param txid Character string. Transaction ID of the output being added to
#'   the offer.
#' @param vout Integer. Output index (vout) of the transaction being added.
#' @param amounts A list specifying the assets or native currency asked for
#'   in exchange for this addition. Format: \code{list(asset_name = quantity, ...)}
#'   or a numeric value for native currency.
#'
#' @return A list with two elements:
#'   \item{hex}{The new partial transaction hex string.}
#'   \item{complete}{Logical; \code{TRUE} if the exchange is now complete.}
#'
#' @examples
#' \dontrun{
#' # Assume 'partial_hex' is a partial exchange from a previous step
#' new <- mc_append_raw_exchange(conn, partial_hex,
#'                               txid = "abc...", vout = 0,
#'                               amounts = list(myasset = 10))
#' }
#'
#' @seealso \code{\link{mc_create_raw_exchange}}, \code{\link{mc_complete_raw_exchange}}
#'
#' @family atomic exchange
#' @export
mc_append_raw_exchange <- function(conn, tx_hex, txid, vout, amounts) {
  mc_rpc(conn, "appendrawexchange", list(tx_hex, txid, as.integer(vout), amounts))
}

#' Finalize an atomic exchange transaction
#'
#' Completes a multi‑party atomic exchange by adding the final input–output
#' pair. After this step, the transaction is fully built and ready to be
#' broadcast.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param tx_hex Character string. Hexadecimal representation of the partial
#'   exchange transaction (should already contain all other parties' contributions).
#' @param txid Character string. Transaction ID of the completing output.
#' @param vout Integer. Output index (vout) of the completing transaction.
#' @param amounts A list specifying the assets or native currency for the
#'   final part of the exchange.
#' @param data Optional metadata. Can be a character string or a list (which
#'   will be converted to JSON and then to hex). This data is embedded in the
#'   transaction.
#'
#' @return Character string. Raw transaction hex ready for sending via
#'   \code{\link{mc_send_raw_transaction}}.
#'
#' @examples
#' \dontrun{
#' final <- mc_complete_raw_exchange(conn, partial_hex,
#'                                   txid = "def...", vout = 1,
#'                                   amounts = list(ETH = 5),
#'                                   data = "exchange complete")
#' }
#'
#' @seealso \code{\link{mc_create_raw_exchange}}, \code{\link{mc_append_raw_exchange}}
#'
#' @family atomic exchange
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
#' Initialises a partial atomic exchange transaction by specifying the first
#' locked output and the desired assets/currency in return. This is the first
#' step in constructing a multi‑party atomic exchange.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param txid Character string. Transaction ID of the locked output (obtained
#'   via \code{\link{mc_prepare_lock_unspent}}).
#' @param vout Integer. Output index (vout) of the locked output.
#' @param amounts A list specifying the assets or native currency asked for
#'   in exchange. Format: \code{list(asset_name = quantity, ...)} or a numeric
#'   value for native currency.
#'
#' @return Character string. Raw partial transaction in hexadecimal.
#'
#' @examples
#' \dontrun{
#' # First, lock some output
#' locked <- mc_prepare_lock_unspent(conn, amounts = list(myasset = 10))
#' # Create the exchange offer
#' offer <- mc_create_raw_exchange(conn, locked$txid, locked$vout,
#'                                 amounts = list(otherasset = 5))
#' }
#'
#' @seealso \code{\link{mc_prepare_lock_unspent}}, \code{\link{mc_append_raw_exchange}}
#'
#' @family atomic exchange
#' @export
mc_create_raw_exchange <- function(conn, txid, vout, amounts) {
  mc_rpc(conn, "createrawexchange", list(txid, as.integer(vout), amounts))
}

#' Decode a raw exchange transaction
#'
#' Parses a raw atomic exchange transaction (partial or complete) and returns
#' a human‑readable representation of its structure, including the involved
#' inputs, outputs, and the assets being exchanged.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param tx_hex Character string. Hexadecimal representation of the exchange
#'   transaction.
#' @param verbose Logical. If \code{TRUE}, lists all individual stages
#'   (contributions) of the exchange. Default is \code{FALSE}.
#'
#' @return A list (or a data frame if \code{verbose}) containing the decoded
#'   exchange details.
#'
#' @examples
#' \dontrun{
#' decoded <- mc_decode_raw_exchange(conn, my_tx_hex)
#' print(decoded)
#' }
#'
#' @seealso \code{\link{mc_create_raw_exchange}}
#'
#' @family atomic exchange
#' @export
mc_decode_raw_exchange <- function(conn, tx_hex, verbose = FALSE) {
  mc_rpc(conn, "decoderawexchange", list(tx_hex, verbose))
}

#' Disable an offer of exchange
#'
#' Invalidates a previously created partial exchange transaction, preventing
#' it from being completed. The transaction is replaced with a disabling
#' transaction that spends the locked output(s) back to the original owner(s).
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param tx_hex Character string. Hexadecimal representation of the exchange
#'   transaction to disable.
#'
#' @return Character string. Transaction ID of the disabling transaction.
#'
#' @examples
#' \dontrun{
#' # After creating an offer, but before completion, you may decide to cancel
#' disable_txid <- mc_disable_raw_transaction(conn, offer_hex)
#' }
#'
#' @seealso \code{\link{mc_prepare_lock_unspent}}
#'
#' @family atomic exchange
#' @export
mc_disable_raw_transaction <- function(conn, tx_hex) {
  mc_rpc(conn, "disablerawtransaction", list(tx_hex))
}

#' Prepare an unspent transaction output for exchange
#'
#' Locks one or more unspent outputs (assets or native currency) to be used
#' as part of an atomic exchange. The locked output is prepared in a way that
#' it can only be spent as part of an atomic exchange transaction.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param amounts A list specifying the assets or native currency to lock.
#'   Format: \code{list(asset_name = quantity, ...)} or a numeric value for
#'   native currency.
#' @param lock Logical. If \code{TRUE} (default), the output is locked for
#'   automatic spending (i.e., it will be used only in the exchange). If
#'   \code{FALSE}, the output is simply prepared but not locked.
#'
#' @return A list with two elements:
#'   \item{txid}{Transaction ID of the prepared/locked output.}
#'   \item{vout}{Output index.}
#'
#' @examples
#' \dontrun{
#' # Lock 10 units of 'myasset' and 0.5 native currency
#' locked <- mc_prepare_lock_unspent(conn, amounts = list(myasset = 10, 0.5))
#' # Now use locked$txid and locked$vout in an exchange
#' }
#'
#' @seealso \code{\link{mc_prepare_lock_unspent_from}}, \code{\link{mc_create_raw_exchange}}
#'
#' @family atomic exchange
#' @export
mc_prepare_lock_unspent <- function(conn, amounts, lock = TRUE) {
  mc_rpc(conn, "preparelockunspent", list(amounts, lock))
}

#' Prepare an unspent output from a specific address
#'
#' Similar to \code{\link{mc_prepare_lock_unspent}}, but allows specifying
#' the source address from which to lock the outputs. This is useful when
#' the node has multiple addresses and you want to control which address's
#' funds are used.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. The address that will provide the
#'   assets/native currency.
#' @param amounts A list specifying the assets or native currency to lock.
#' @param lock Logical. If \code{TRUE} (default), the output is locked for
#'   automatic spending.
#'
#' @return A list with \code{txid} and \code{vout}.
#'
#' @examples
#' \dontrun{
#' locked <- mc_prepare_lock_unspent_from(conn, "1A...",
#'                                        amounts = list(myasset = 5))
#' }
#'
#' @seealso \code{\link{mc_prepare_lock_unspent}}
#'
#' @family atomic exchange
#' @export
mc_prepare_lock_unspent_from <- function(conn, from_address, amounts, lock = TRUE) {
  mc_rpc(conn, "preparelockunspentfrom", list(from_address, amounts, lock))
}
