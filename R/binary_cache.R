#' Create a new binary cache item
#'
#' Creates an empty item (file) in the node's binary cache and returns its
#' unique identifier. Binary cache items are temporary storage for binary data
#' that can be used in transactions or passed between nodes.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return A character string identifier (filename) for the newly created cache item.
#'
#' @examples
#' \dontrun{
#' id <- mc_create_binary_cache(conn)
#' }
#'
#' @seealso \code{\link{mc_append_binary_cache}} to add data,
#'   \code{\link{mc_delete_binary_cache}} to remove.
#'
#' @family binary cache
#' @export
mc_create_binary_cache <- function(conn) {
  mc_rpc(conn, "createbinarycache")
}

#' Append data to a binary cache item
#'
#' Appends data to an existing binary cache item. If \code{data = ""}
#' (the default), the RPC call returns the current size without adding new data.
#' The \code{data} argument can be provided as a hex string, or as a list
#' with a \code{text} or \code{json} component that will be automatically converted.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param identifier Character string. The cache item identifier returned by
#'   \code{\link{mc_create_binary_cache}}.
#' @param data Data to append. Can be:
#'   * a character string of hex data,
#'   * a list with element \code{text} (will be converted to hex),
#'   * a list with element \code{json} (will be converted to JSON and then to hex),
#'   * \code{""} (default) returns the current size without appending.
#'
#' @return Integer. The resulting size of the cache item in bytes after appending
#'   (or the current size if \code{data = ""}).
#'
#' @examples
#' \dontrun{
#' id <- mc_create_binary_cache(conn)
#'
#' # Append text data
#' mc_append_binary_cache(conn, id, list(text = "Hello, world!"))
#'
#' # Append JSON data
#' mc_append_binary_cache(conn, id, list(json = list(key = "value")))
#'
#' # Get current size without appending
#' size <- mc_append_binary_cache(conn, id)
#' }
#'
#' @seealso \code{\link{mc_create_binary_cache}}, \code{\link{mc_delete_binary_cache}}
#'
#' @family binary cache
#' @export
mc_append_binary_cache <- function(conn, identifier, data = "") {
  # Если передана пустая строка, RPC вернет текущий размер без добавления данных
  mc_rpc(conn, "appendbinarycache", list(identifier, data))
}

#' Delete an item from the binary cache
#'
#' Removes a previously created binary cache item. Once deleted, the identifier
#' becomes invalid and cannot be used further.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param identifier Character string. The cache item identifier to remove.
#'
#' @return Invisibly returns \code{NULL} on success; throws an error if the
#'   item does not exist or cannot be deleted.
#'
#' @examples
#' \dontrun{
#' id <- mc_create_binary_cache(conn)
#' # ... use the cache item ...
#' mc_delete_binary_cache(conn, id)
#' }
#'
#' @seealso \code{\link{mc_create_binary_cache}}, \code{\link{mc_append_binary_cache}}
#'
#' @family binary cache
#' @export
mc_delete_binary_cache <- function(conn, identifier) {
  mc_rpc(conn, "deletebinarycache", list(identifier))
}

#' Extract transaction output data to binary cache
#'
#' Copies data directly from a blockchain transaction output into a binary cache
#' item. This is efficient for retrieving binary data stored in a transaction
#' (e.g., via \code{\link{mc_publish}}) without having to decode it in R.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param identifier Character string. Target cache item identifier.
#'   The cache item must be empty (created but not yet written to).
#' @param txid Character string. Transaction ID containing the output.
#' @param vout Integer. Output index (vout) of the transaction to extract.
#' @param count_bytes Integer (optional). Number of bytes to extract.
#'   If \code{NULL} (default), the entire output data is copied.
#' @param start_byte Integer (optional). Byte offset from which to start copying.
#'   Default is \code{0} (beginning of the output data).
#'
#' @return Integer. The resulting size of the cache item after extraction.
#'
#' @examples
#' \dontrun{
#' # Create an empty cache item
#' id <- mc_create_binary_cache(conn)
#'
#' # Copy the entire data from a transaction output
#' size <- mc_txout_to_binary_cache(conn, id, txid = "abc...", vout = 0)
#'
#' # Copy only the first 100 bytes
#' size <- mc_txout_to_binary_cache(conn, id, txid = "abc...", vout = 0,
#'                                  count_bytes = 100)
#' }
#'
#' @seealso \code{\link{mc_create_binary_cache}}, \code{\link{mc_append_binary_cache}}
#'
#' @family binary cache
#' @export
mc_txout_to_binary_cache <- function(conn, identifier, txid, vout, count_bytes = NULL, start_byte = 0) {
  params <- list(identifier, txid, as.integer(vout))
  
  if (!is.null(count_bytes)) {
    params <- c(params, list(as.integer(count_bytes), as.integer(start_byte)))
  }
  
  mc_rpc(conn, "txouttobinarycache", params)
}
