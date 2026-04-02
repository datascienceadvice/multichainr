#' Send payment or assets to an address
#'
#' Sends a payment (native currency or assets) to a specified address. This is
#' a general‑purpose sending function that can handle multiple asset types in
#' a single transaction.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param address Character string. Recipient address.
#' @param amounts Either a numeric value (for native currency) or a named list
#'   specifying assets and quantities, e.g., \code{list(asset1 = 10, asset2 = 5)}.
#'   For metadata, see \code{\link{mc_send_with_data}}.
#' @param comment Character string. Optional transaction comment (stored on chain).
#' @param comment_to Character string. Optional comment‑to field.
#'
#' @return A character string containing the transaction ID (txid).
#'
#' @examples
#' \dontrun{
#' # Send 1.5 native coins
#' txid <- mc_send(conn, "1A...", 1.5)
#'
#' # Send assets only
#' txid <- mc_send(conn, "1A...", list(myasset = 100))
#'
#' # Send both native and assets
#' txid <- mc_send(conn, "1A...", list(0.5, myasset = 50))
#' }
#'
#' @seealso \code{\link{mc_send_from}} to specify the sender,
#'   \code{\link{mc_send_asset}} for single‑asset convenience.
#'
#' @family transactions
#' @export
mc_send <- function(conn, address, amounts, comment = "", comment_to = "") {
  mc_rpc(conn, "send", list(address, amounts, comment, comment_to))
}

#' Send a single asset to an address
#'
#' Convenience function to send a single asset (or native currency) to an address.
#' Equivalent to \code{\link{mc_send}} but with a simpler interface.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param address Character string. Recipient address.
#' @param asset Character string. Asset name, reference, or issuance transaction ID.
#' @param quantity Numeric. Amount of the asset to send.
#' @param native_amount Numeric. Amount of native currency to send (default 0).
#' @param comment Character string. Optional transaction comment.
#' @param comment_to Character string. Optional comment‑to field.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Send 100 units of "myasset"
#' txid <- mc_send_asset(conn, "1A...", "myasset", 100)
#'
#' # Send asset along with 0.5 native coins
#' txid <- mc_send_asset(conn, "1A...", "myasset", 100, native_amount = 0.5)
#' }
#'
#' @seealso \code{\link{mc_send_asset_from}} to specify sender,
#'   \code{\link{mc_send}} for multiple assets.
#'
#' @family transactions
#' @export
mc_send_asset <- function(conn, address, asset, quantity, native_amount = 0, comment = "", comment_to = "") {
  params <- list(address, asset, quantity, as.numeric(native_amount), comment, comment_to)
  mc_rpc(conn, "sendasset", params)
}

#' Send a single asset from a specific address
#'
#' Sends an asset (or native currency) from a specific sender address.
#' Useful when the node has multiple addresses and you want to control which
#' address the funds are taken from.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. Sender address (must belong to the node's wallet).
#' @param to_address Character string. Recipient address.
#' @param asset Character string. Asset name, reference, or issuance transaction ID.
#' @param quantity Numeric. Amount of the asset to send.
#' @param native_amount Numeric. Amount of native currency to send (default 0).
#' @param comment Character string. Optional transaction comment.
#' @param comment_to Character string. Optional comment‑to field.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Send from a specific address
#' txid <- mc_send_asset_from(conn, "1A...", "1B...", "myasset", 50)
#' }
#'
#' @seealso \code{\link{mc_send_asset}}, \code{\link{mc_send_from}}.
#'
#' @family transactions
#' @export
mc_send_asset_from <- function(conn, from_address, to_address, asset, quantity, native_amount = 0, comment = "", comment_to = "") {
  params <- list(from_address, to_address, asset, as.numeric(quantity), as.numeric(native_amount), comment, comment_to)
  mc_rpc(conn, "sendassetfrom", params)
}

#' Send payment from a specific address
#'
#' Sends a payment (native currency or assets) from a specific sender address.
#' This is the counterpart of \code{\link{mc_send}} for multi‑asset transactions
#' with a chosen source address.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. Sender address.
#' @param to_address Character string. Recipient address.
#' @param amounts Either a numeric value (native) or a named list of assets.
#' @param comment Character string. Optional transaction comment.
#' @param comment_to Character string. Optional comment‑to field.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Send 2 native coins from one address to another
#' txid <- mc_send_from(conn, "1A...", "1B...", 2)
#'
#' # Send assets from a specific address
#' txid <- mc_send_from(conn, "1A...", "1B...", list(myasset = 100, other = 50))
#' }
#'
#' @seealso \code{\link{mc_send}}, \code{\link{mc_send_asset_from}}.
#'
#' @family transactions
#' @export
mc_send_from <- function(conn, from_address, to_address, amounts, comment = "", comment_to = "") {
  mc_rpc(conn, "sendfrom", list(from_address, to_address, amounts, comment, comment_to))
}

#' Send payment with inline metadata
#'
#' Sends a transaction that includes arbitrary data (metadata) attached to the
#' output. The data can be text, JSON, or any binary data (hex‑encoded).
#' This is useful for storing small amounts of information on the blockchain.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param address Character string. Recipient address.
#' @param amounts Either a numeric value (native) or a named list of assets.
#' @param data Data to embed. Can be a character string (will be hex‑encoded),
#'   a list (will be converted to JSON then hex), or raw binary (not directly).
#'   The function automatically converts lists to JSON and then to hex.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Send with a text note
#' txid <- mc_send_with_data(conn, "1A...", 0.1, "Hello, blockchain!")
#'
#' # Send with structured JSON metadata
#' metadata <- list(id = 123, action = "transfer", tag = "payment")
#' txid <- mc_send_with_data(conn, "1A...", list(myasset = 10), metadata)
#' }
#'
#' @seealso \code{\link{mc_send_with_data_from}} to specify sender,
#'   \code{\link{mc_send}} for simple payments.
#'
#' @family transactions
#' @export
mc_send_with_data <- function(conn, address, amounts, data) {
  if (is.list(data)) {
    data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  }
  data_hex <- paste(charToRaw(as.character(data)), collapse = "")
  
  mc_rpc(conn, "sendwithdata", list(address, amounts, data_hex))
}

#' Send payment from specific address with metadata
#'
#' Sends a transaction with inline metadata from a specified sender address.
#' Combines the capabilities of \code{\link{mc_send_from}} and
#' \code{\link{mc_send_with_data}}.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. Sender address.
#' @param to_address Character string. Recipient address.
#' @param amounts Either a numeric value (native) or a named list of assets.
#' @param data Data to embed. Can be a string, list (converted to JSON), etc.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Send from a specific address with metadata
#' txid <- mc_send_with_data_from(conn, "1A...", "1B...", 0.5,
#'                                list(reference = "invoice123"))
#' }
#'
#' @seealso \code{\link{mc_send_with_data}}, \code{\link{mc_send_from}}.
#'
#' @family transactions
#' @export
mc_send_with_data_from <- function(conn, from_address, to_address, amounts, data) {
  if (is.list(data)) {
    data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  }
  data_hex <- paste(charToRaw(as.character(data)), collapse = "")
  
  mc_rpc(conn, "sendwithdatafrom", list(from_address, to_address, amounts, data_hex))
}
