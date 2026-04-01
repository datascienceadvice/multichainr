#' Create a new binary cache item
#' 
#' Creates an empty item (file) in the node's binary cache and returns its identifier.
#' @param conn Connection object.
#' @return A string identifier (filename).
#' @export
mc_create_binary_cache <- function(conn) {
  mc_rpc(conn, "createbinarycache")
}

#' Append data to a binary cache item
#' 
#' @param conn Connection object.
#' @param identifier The cache item identifier (from mc_create_binary_cache).
#' @param data Data to append. Can be a hex string, or a list like list(text="...") or list(json=...).
#' @return The resulting size of the cache item in bytes.
#' @export
mc_append_binary_cache <- function(conn, identifier, data = "") {
  # Если передана пустая строка, RPC вернет текущий размер без добавления данных
  mc_rpc(conn, "appendbinarycache", list(identifier, data))
}

#' Delete an item from the binary cache
#' 
#' @param conn Connection object.
#' @param identifier The cache item identifier to remove.
#' @return NULL on success.
#' @export
mc_delete_binary_cache <- function(conn, identifier) {
  mc_rpc(conn, "deletebinarycache", list(identifier))
}

#' Extract transaction output data to binary cache
#' 
#' Copies data from a blockchain transaction output directly into a binary cache item.
#' 
#' @param conn Connection object.
#' @param identifier Target cache item identifier (must be empty).
#' @param txid Transaction ID.
#' @param vout Output index.
#' @param count_bytes Optional number of bytes to extract.
#' @param start_byte Optional offset in bytes.
#' @return The resulting size of the cache item.
#' @export
mc_txout_to_binary_cache <- function(conn, identifier, txid, vout, count_bytes = NULL, start_byte = 0) {
  params <- list(identifier, txid, as.integer(vout))
  
  if (!is.null(count_bytes)) {
    params <- c(params, list(as.integer(count_bytes), as.integer(start_byte)))
  }
  
  mc_rpc(conn, "txouttobinarycache", params)
}