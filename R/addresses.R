#' Add a multi-signature address
#' 
#' Creates a multi-signature address from a list of keys and required signatures.
#' 
#' @param conn Connection object.
#' @param n_required Number of required signatures (integer).
#' @param keys A character vector of public keys or addresses.
#' @return A string containing the multisig address.
#' @export
mc_add_multisig_address <- function(conn, n_required, keys) {
  params <- list(as.integer(n_required), as.list(keys))
  mc_rpc(conn, "addmultisigaddress", params)
}

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

#' Import a watch-only address
#' 
#' Adds an address (without a private key) to the node's wallet for monitoring.
#' 
#' @param conn Connection object.
#' @param address The wallet address to import.
#' @param label Optional label for the address.
#' @param rescan If TRUE (default), the node will scan the blockchain for transactions 
#' associated with this address.
#' @return NULL on success.
#' @export
mc_import_address <- function(conn, address, label = "", rescan = TRUE) {
  mc_rpc(conn, "importaddress", list(address, label, rescan))
}

#' List addresses in the wallet
#' 
#' Returns information about the addresses in the current node's wallet.
#' 
#' @param conn Connection object.
#' @param addresses A specific address or vector of addresses to filter. Default "*" (all).
#' @param verbose If TRUE, provides detailed information.
#' @param count Optional. Number of addresses to list.
#' @param start Optional. Offset for the list.
#' @return A data frame containing address information.
#' @export
mc_list_addresses <- function(conn, addresses = "*", verbose = FALSE, count = NULL, start = NULL) {
  params <- list(addresses, verbose)
  
  if (!is.null(count)) {
    params <- c(params, list(as.integer(count)))
    if (!is.null(start)) {
      params <- c(params, list(as.integer(start)))
    }
  }
  
  res <- mc_rpc(conn, "listaddresses", params)
  rpc_res_to_df(res)
}

#' Create new key pairs
#' 
#' Generates one or more public/private key pairs. These are NOT stored 
#' in the node's wallet.
#' 
#' @param conn Connection object.
#' @param count Number of key pairs to generate (default 1).
#' @return A data frame with columns: address, pubkey, and privkey.
#' @export
mc_create_keypairs <- function(conn, count = 1) {
  res <- mc_rpc(conn, "createkeypairs", list(as.integer(count)))
  rpc_res_to_df(res)
}

#' Create multi-signature address (external)
#' 
#' Creates a P2SH multi-signature address without adding it to the wallet.
#' 
#' @param conn Connection object.
#' @param n_required Number of required signatures.
#' @param keys A character vector of public keys or addresses.
#' @return A list containing the 'address' and 'redeemScript'.
#' @export
mc_create_multisig <- function(conn, n_required, keys) {
  params <- list(as.integer(n_required), as.list(keys))
  mc_rpc(conn, "createmultisig", params)
}

#' Validate or inspect an address
#' 
#' Returns information about a given address, private key, or public key.
#' 
#' @param conn Connection object.
#' @param address_or_key A string (address, privkey, or pubkey).
#' @return A list of details including validity, address, and ownership status.
#' @export
mc_validate_address <- function(conn, address_or_key) {
  mc_rpc(conn, "validateaddress", list(address_or_key))
}