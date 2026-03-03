#' Export private key for an address
#' 
#' Returns the private key in WIF (Wallet Import Format) for a specific address.
#' @param conn Connection object.
#' @param address The wallet address to export.
#' @return A string containing the private key.
#' @export
mc_export_private_key <- function(conn, address) {
  res <- mc_rpc(conn, "dumpprivkey", list(address))
  
  return(trimws(as.character(unname(res))))
}

#' Import a private key to the node
#' 
#' Adds a private key (in WIF format) to the current node's wallet.
#' @param conn Connection object.
#' @param privkey The private key to import (from mc_export_private_key).
#' @param label Optional label for the address.
#' @param rescan If TRUE (default), the node will scan the blockchain for transactions 
#' associated with this key. This may take a long time on large chains.
#' @return NULL on success.
#' @export
mc_import_private_key <- function(conn, privkey, label = "", rescan = TRUE) {
  clean_key <- trimws(as.character(unname(privkey)))
  
  if (nchar(clean_key) == 0) stop("Private key is empty")
  
  mc_rpc(conn, "importprivkey", list(clean_key, label, rescan))
}