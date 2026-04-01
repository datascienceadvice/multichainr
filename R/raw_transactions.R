#' Create a raw transaction
#'
#' Creates a raw (unsigned) transaction from a list of inputs and outputs.
#' This is the first step in building a custom transaction before signing and
#' broadcasting.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param inputs A list of input objects, each containing:
#'   \itemize{
#'     \item \code{txid} – Transaction ID of the UTXO.
#'     \item \code{vout} – Output index (vout) of the UTXO.
#'   }
#' @param outputs A named list (or list of named lists) mapping addresses to
#'   amounts. Example: \code{list("address1" = 0.5, "address2" = list(asset = 10))}.
#' @param data Optional array of metadata. Each element can be a string or a list
#'   (which will be converted to JSON then hex). The data is embedded in the
#'   transaction outputs.
#' @param action Optional action string: \code{"lock"} (lock inputs),
#'   \code{"sign"} (sign the transaction), \code{"lock,sign"} (both), or
#'   \code{"send"} (sign and send). Default is \code{""} (just create).
#'
#' @return A character string containing the raw transaction hex (if no action)
#'   or a list with \code{hex} and \code{complete} if action includes signing.
#'
#' @examples
#' \dontrun{
#' # Build a simple transaction
#' inputs <- list(list(txid = "abc...", vout = 0))
#' outputs <- list("1A..." = 1.0)
#' tx_hex <- mc_create_raw_transaction(conn, inputs, outputs)
#'
#' # With metadata
#' tx_hex <- mc_create_raw_transaction(conn, inputs, outputs,
#'                                     data = list("Hello", list(key = "value")))
#' }
#'
#' @seealso \code{\link{mc_sign_raw_transaction}}, \code{\link{mc_send_raw_transaction}}
#'
#' @family raw transactions
#' @export
mc_create_raw_transaction <- function(conn, inputs, outputs, data = list(), action = "") {
  inputs <- lapply(inputs, function(x) {
    x$vout <- as.integer(x$vout)
    x
  })
  
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
#' Creates a raw transaction that is automatically funded from a specified
#' address. This is a convenience function that selects UTXOs from the given
#' address and builds the transaction.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. The address that will fund the transaction.
#' @param to_amounts A named list mapping recipient addresses to amounts
#'   (e.g., \code{list("1A..." = 0.5, "1B..." = list(asset = 10))}).
#' @param data Optional array of metadata (strings or lists; will be hex‑encoded).
#' @param action Optional action: \code{"send"}, \code{"sign"}, etc.
#'
#' @return A character string (raw transaction hex) or a list with \code{hex}
#'   and \code{complete} if signing is requested.
#'
#' @examples
#' \dontrun{
#' # Send 1.0 native coin to address
#' tx_hex <- mc_create_raw_send_from(conn, "1A...", list("1B..." = 1.0))
#'
#' # Send asset and metadata
#' tx_hex <- mc_create_raw_send_from(conn, "1A...",
#'                                   list("1B..." = list(myasset = 50)),
#'                                   data = list("payment", list(ref = 123)))
#' }
#'
#' @seealso \code{\link{mc_create_raw_transaction}}
#'
#' @family raw transactions
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
#' Appends a change output to a raw transaction. This is useful when the
#' transaction inputs exceed the required output amounts; the change is sent
#' back to the specified address.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param tx_hex Character string. The raw transaction hex to which change is added.
#' @param address Character string. The address that will receive the change.
#' @param native_fee Optional numeric. Native currency fee to be deducted from
#'   the change. If provided, the change output is reduced accordingly.
#'
#' @return A character string containing the updated raw transaction hex.
#'
#' @examples
#' \dontrun{
#' # Add change to a raw transaction
#' updated_hex <- mc_append_raw_change(conn, tx_hex, "1A...")
#' }
#'
#' @seealso \code{\link{mc_append_raw_data}}, \code{\link{mc_append_raw_transaction}}
#'
#' @family raw transactions
#' @export
mc_append_raw_change <- function(conn, tx_hex, address, native_fee = NULL) {
  params <- list(tx_hex, address)
  if (!is.null(native_fee)) params <- c(params, list(as.numeric(native_fee)))
  mc_rpc(conn, "appendrawchange", params)
}

#' Add metadata to a raw transaction
#'
#' Appends arbitrary data (metadata) to a raw transaction. The data is embedded
#' in an output with zero native value.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param tx_hex Character string. The raw transaction hex.
#' @param data Data to embed. Can be a string or a list (converted to JSON then hex).
#'
#' @return A character string containing the updated raw transaction hex.
#'
#' @examples
#' \dontrun{
#' # Add a text note
#' updated_hex <- mc_append_raw_data(conn, tx_hex, "This is a note.")
#'
#' # Add JSON metadata
#' updated_hex <- mc_append_raw_data(conn, tx_hex, list(tag = "invoice", id = 123))
#' }
#'
#' @seealso \code{\link{mc_append_raw_change}}, \code{\link{mc_append_raw_transaction}}
#'
#' @family raw transactions
#' @export
mc_append_raw_data <- function(conn, tx_hex, data) {
  if (is.list(data)) data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  data_hex <- paste(charToRaw(as.character(data)), collapse = "")
  mc_rpc(conn, "appendrawdata", list(tx_hex, data_hex))
}

#' Add inputs and outputs to a raw transaction
#'
#' Appends additional inputs and outputs to an existing raw transaction.
#' This is useful for building multi‑party transactions or adding extra
#' components after creation.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param tx_hex Character string. The raw transaction hex to which inputs/outputs are added.
#' @param inputs A list of input objects (each with \code{txid} and \code{vout}).
#' @param outputs A named list of outputs (mapping addresses to amounts).
#'
#' @return A character string containing the updated raw transaction hex.
#'
#' @examples
#' \dontrun{
#' # Add another input and output
#' new_input <- list(list(txid = "def...", vout = 1))
#' new_output <- list("1C..." = 0.2)
#' updated_hex <- mc_append_raw_transaction(conn, tx_hex,
#'                                          inputs = new_input,
#'                                          outputs = new_output)
#' }
#'
#' @seealso \code{\link{mc_create_raw_transaction}}, \code{\link{mc_append_raw_change}}
#'
#' @family raw transactions
#' @export
mc_append_raw_transaction <- function(conn, tx_hex, inputs = list(), outputs = list()) {
  inputs <- lapply(inputs, function(x) {
    x$vout <- as.integer(x$vout)
    x
  })
  mc_rpc(conn, "appendrawtransaction", list(tx_hex, inputs, outputs))
}

#' Decode a raw transaction hex
#'
#' Parses a raw transaction hex string into a human‑readable structure,
#' showing inputs, outputs, amounts, metadata, and other transaction details.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param tx_hex Character string. The raw transaction hex to decode.
#'
#' @return A list with decoded transaction information (inputs, outputs, etc.).
#'
#' @examples
#' \dontrun{
#' decoded <- mc_decode_raw_transaction(conn, tx_hex)
#' print(decoded$vin)
#' }
#'
#' @seealso \code{\link{mc_get_raw_transaction}}
#'
#' @family raw transactions
#' @export
mc_decode_raw_transaction <- function(conn, tx_hex) {
  mc_rpc(conn, "decoderawtransaction", list(tx_hex))
}

#' Sign a raw transaction
#'
#' Signs a raw transaction using the node's wallet or provided private keys.
#' Returns the signed hex and a boolean indicating whether all inputs are signed.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param tx_hex Character string. The raw transaction hex to sign.
#' @param parents Optional list of parent outputs for signing, each containing
#'   \code{txid}, \code{vout}, and \code{scriptPubKey}.
#' @param private_keys Optional character vector of private keys in Wallet Import
#'   Format (WIF). If provided, these are used instead of the node's wallet.
#' @param sighashtype Character string. Signature hash type (default \code{"ALL"}).
#'
#' @return A list with two elements:
#'   \item{hex}{The signed raw transaction hex (if all inputs are signed).}
#'   \item{complete}{Logical; \code{TRUE} if all inputs are signed.}
#'
#' @examples
#' \dontrun{
#' # Sign using the node's wallet
#' signed <- mc_sign_raw_transaction(conn, tx_hex)
#'
#' # Sign with explicit private keys
#' signed <- mc_sign_raw_transaction(conn, tx_hex,
#'                                   private_keys = c("L5...", "K3..."))
#' }
#'
#' @seealso \code{\link{mc_send_raw_transaction}}, \code{\link{mc_create_raw_transaction}}
#'
#' @family raw transactions
#' @export
mc_sign_raw_transaction <- function(conn, tx_hex, parents = NULL, private_keys = NULL, sighashtype = "ALL") {
  params <- list(tx_hex, parents, private_keys, sighashtype)
  mc_rpc(conn, "signrawtransaction", params)
}

#' Send a signed raw transaction to the network
#'
#' Broadcasts a signed raw transaction to the blockchain network. The
#' transaction must be complete (all inputs signed) before sending.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param tx_hex Character string. The signed raw transaction hex to send.
#'
#' @return A character string containing the transaction ID (txid).
#'
#' @examples
#' \dontrun{
#' # Create, sign, and send a transaction
#' tx_hex <- mc_create_raw_transaction(conn, inputs, outputs)
#' signed <- mc_sign_raw_transaction(conn, tx_hex)
#' if (signed$complete) {
#'   txid <- mc_send_raw_transaction(conn, signed$hex)
#' }
#' }
#'
#' @seealso \code{\link{mc_sign_raw_transaction}}, \code{\link{mc_create_raw_transaction}}
#'
#' @family raw transactions
#' @export
mc_send_raw_transaction <- function(conn, tx_hex) {
  mc_rpc(conn, "sendrawtransaction", list(tx_hex))
}
