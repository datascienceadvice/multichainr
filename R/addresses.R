#' Get node wallet addresses
#' 
#' Returns all addresses owned by the current node.
#' @param conn Connection object.
#' @param verbose If TRUE, returns detailed information about addresses.
#' @export
mc_get_addresses <- function(conn, verbose = FALSE) {
  res <- mc_rpc(conn, "getaddresses", list(verbose))
  if (verbose) return(res)
  return(as.character(unlist(res)))
}

#' Create new wallet address
#' 
#' @param conn Connection object.
#' @export
mc_get_new_address <- function(conn) {
  mc_rpc(conn, "getnewaddress")
}