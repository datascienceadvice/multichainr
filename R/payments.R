#' Send payment or assets to an address
#' 
#' @param conn Connection object.
#' @param address Recipient address.
#' @param amounts Numeric (native currency) or a list of assets/metadata.
#' @param comment Optional transaction comment.
#' @param comment_to Optional comment-to.
#' @export
mc_send <- function(conn, address, amounts, comment = "", comment_to = "") {
  mc_rpc(conn, "send", list(address, amounts, comment, comment_to))
}

#' Send a single asset to an address
#' 
#' @param conn Connection object.
#' @param address Recipient address.
#' @param asset Asset name, ref or txid.
#' @param quantity Amount of asset to send.
#' @param native_amount Optional native currency to send (default 0).
#' @param comment Optional comment.
#' @param comment_to Optional comment-to.
#' @export
mc_send_asset <- function(conn, address, asset, quantity, native_amount = 0, comment = "", comment_to = "") {
  params <- list(address, asset, as.numeric(quantity), as.numeric(native_amount), comment, comment_to)
  mc_rpc(conn, "sendasset", params)
}

#' Send a single asset from a specific address
#' 
#' @param conn Connection object.
#' @param from_address Sender address.
#' @param to_address Recipient address.
#' @param asset Asset name, ref or txid.
#' @param quantity Amount of asset.
#' @param native_amount Optional native currency.
#' @param comment Optional comment.
#' @param comment_to Optional comment-to.
#' @export
mc_send_asset_from <- function(conn, from_address, to_address, asset, quantity, native_amount = 0, comment = "", comment_to = "") {
  params <- list(from_address, to_address, asset, as.numeric(quantity), as.numeric(native_amount), comment, comment_to)
  mc_rpc(conn, "sendassetfrom", params)
}

#' Send payment from a specific address
#' 
#' @param conn Connection object.
#' @param from_address Sender address.
#' @param to_address Recipient address.
#' @param amounts Numeric (native) or list of assets/metadata.
#' @param comment Optional comment.
#' @param comment_to Optional comment-to.
#' @export
mc_send_from <- function(conn, from_address, to_address, amounts, comment = "", comment_to = "") {
  mc_rpc(conn, "sendfrom", list(from_address, to_address, amounts, comment, comment_to))
}

#' Send payment with inline metadata
#' 
#' @param conn Connection object.
#' @param address Recipient address.
#' @param amounts Numeric (native) or list of assets.
#' @param data Data string or list (will be converted to hex).
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
#' @param conn Connection object.
#' @param from_address Sender address.
#' @param to_address Recipient address.
#' @param amounts Numeric (native) or list of assets.
#' @param data Data string or list (will be converted to hex).
#' @export
mc_send_with_data_from <- function(conn, from_address, to_address, amounts, data) {
  if (is.list(data)) {
    data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  }
  data_hex <- paste(charToRaw(as.character(data)), collapse = "")
  
  mc_rpc(conn, "sendwithdatafrom", list(from_address, to_address, amounts, data_hex))
}