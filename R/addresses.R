#' Add a multi-signature address
#'
#' Creates a multi-signature address and adds it to the node's wallet.
#' The address is a Pay‑to‑Script‑Hash (P2SH) address that requires a specified
#' number of signatures from the provided keys.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param n_required Integer. Number of signatures required to spend funds.
#' @param keys Character vector. Public keys or addresses that will be part of
#'   the multi-signature set.
#'
#' @return A character string containing the multi-signature address.
#'
#' @examples
#' \dontrun{
#' # Assume connection 'conn' is already established
#' addr <- mc_add_multisig_address(conn, n_required = 2,
#'                                 keys = c("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa",
#'                                          "1BvBMSEYstWetqTFn5Au4m4GFg7xJaNVN2"))
#' }
#'
#' @seealso \code{\link{mc_create_multisig}} to create a multisig address
#'   without adding it to the wallet.
#'
#' @family address operations
#' @export
mc_add_multisig_address <- function(conn, n_required, keys) {
  params <- list(as.integer(n_required), as.list(keys))
  mc_rpc(conn, "addmultisigaddress", params)
}

#' Get node wallet addresses
#'
#' Returns all addresses owned by the current node. If \code{verbose = TRUE},
#' detailed information (including balances and transactions) is returned.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param verbose Logical. If \code{TRUE}, returns a list with detailed
#'   information for each address. If \code{FALSE} (default), returns a
#'   character vector of addresses.
#'
#' @return If \code{verbose = FALSE}: a character vector of addresses.
#'   If \code{verbose = TRUE}: a list (or data frame) with details.
#'
#' @examples
#' \dontrun{
#' # Get all addresses (simple list)
#' addresses <- mc_get_addresses(conn)
#'
#' # Get detailed information
#' details <- mc_get_addresses(conn, verbose = TRUE)
#' }
#'
#' @seealso \code{\link{mc_list_addresses}} for more flexible listing options.
#'
#' @family address operations
#' @export
mc_get_addresses <- function(conn, verbose = FALSE) {
  res <- mc_rpc(conn, "getaddresses", list(verbose))
  if (verbose) return(res)
  return(as.character(unlist(res)))
}

#' Create new wallet address
#'
#' Generates a new address (with a private key) and adds it to the node's wallet.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return A character string with the newly created address.
#'
#' @examples
#' \dontrun{
#' new_addr <- mc_get_new_address(conn)
#' print(new_addr)
#' }
#'
#' @seealso \code{\link{mc_create_keypairs}} to generate key pairs not stored
#'   in the wallet.
#'
#' @family address operations
#' @export
mc_get_new_address <- function(conn) {
  mc_rpc(conn, "getnewaddress")
}

#' Import a watch-only address
#'
#' Adds an address (without a private key) to the node's wallet for monitoring.
#' The node will be able to see transactions involving this address, but cannot
#' spend funds from it.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param address Character string. The wallet address to import.
#' @param label Character string (optional). A label to assign to the address.
#'   Default is \code{""} (no label).
#' @param rescan Logical. If \code{TRUE} (default), the node will scan the
#'   blockchain for transactions associated with this address.
#'
#' @return Invisibly returns \code{NULL} on success; throws an error if the
#'   import fails.
#'
#' @examples
#' \dontrun{
#' mc_import_address(conn, "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa",
#'                   label = "donation", rescan = TRUE)
#' }
#'
#' @seealso \code{\link{mc_validate_address}} to check address validity.
#'
#' @family address operations
#' @export
mc_import_address <- function(conn, address, label = "", rescan = TRUE) {
  mc_rpc(conn, "importaddress", list(address, label, rescan))
}

#' List addresses in the wallet
#'
#' Returns information about the addresses in the current node's wallet.
#' This is a more flexible version of \code{\link{mc_get_addresses}}, allowing
#' filtering, pagination, and optional verbosity.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param addresses A character vector of addresses to filter, or \code{"*"}
#'   (default) to list all addresses.
#' @param verbose Logical. If \code{TRUE}, returns detailed information for each
#'   address.
#' @param count Integer (optional). Maximum number of addresses to return.
#' @param start Integer (optional). Offset for pagination.
#'
#' @return A data frame (created by \code{rpc_res_to_df}) containing address
#'   information. The exact columns depend on the \code{verbose} setting, but
#'   typically include \code{address}, \code{label}, \code{balance}, etc.
#'
#' @examples
#' \dontrun{
#' # List all addresses (simple)
#' all_addr <- mc_list_addresses(conn)
#'
#' # List detailed information for specific addresses
#' details <- mc_list_addresses(conn,
#'                              addresses = c("1A...", "1B..."),
#'                              verbose = TRUE)
#'
#' # Paginate results
#' first_10 <- mc_list_addresses(conn, count = 10)
#' next_10  <- mc_list_addresses(conn, count = 10, start = 10)
#' }
#'
#' @seealso \code{\link{mc_get_addresses}} for a simpler version.
#'
#' @family address operations
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
#' Generates one or more public/private key pairs. These keys are \emph{not}
#' stored in the node's wallet, so they must be kept secure by the user.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param count Integer. Number of key pairs to generate. Default is \code{1}.
#'
#' @return A data frame with three columns:
#'   \item{address}{The public address derived from the key pair.}
#'   \item{pubkey}{The public key.}
#'   \item{privkey}{The private key (keep this secret!).}
#'
#' @examples
#' \dontrun{
#' # Generate a single key pair
#' keys <- mc_create_keypairs(conn)
#' print(keys)
#'
#' # Generate 5 key pairs
#' keys5 <- mc_create_keypairs(conn, count = 5)
#' }
#'
#' @seealso \code{\link{mc_get_new_address}} to create an address stored in the
#'   wallet.
#'
#' @family address operations
#' @export
mc_create_keypairs <- function(conn, count = 1) {
  res <- mc_rpc(conn, "createkeypairs", list(as.integer(count)))
  rpc_res_to_df(res)
}

#' Create multi-signature address (external)
#'
#' Creates a Pay‑to‑Script‑Hash (P2SH) multi-signature address without adding
#' it to the wallet. The address can be used in transactions that require
#' multiple signatures, but the node cannot spend funds from it unless the
#' private keys are also imported.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param n_required Integer. Number of required signatures.
#' @param keys Character vector. Public keys or addresses.
#'
#' @return A list with two components:
#'   \item{address}{The multi-signature address.}
#'   \item{redeemScript}{The redeem script (needed for spending).}
#'
#' @examples
#' \dontrun{
#' multisig <- mc_create_multisig(conn, n_required = 2,
#'                                keys = c("pubkey1", "pubkey2", "pubkey3"))
#' cat("Multisig address:", multisig$address)
#' }
#'
#' @seealso \code{\link{mc_add_multisig_address}} to create and add the address
#'   to the wallet.
#'
#' @family address operations
#' @export
mc_create_multisig <- function(conn, n_required, keys) {
  params <- list(as.integer(n_required), as.list(keys))
  mc_rpc(conn, "createmultisig", params)
}

#' Validate or inspect an address
#'
#' Returns information about a given address, private key, or public key.
#' Useful for checking whether an address is valid, whether it belongs to the
#' current node, and for inspecting its associated redeem script.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param address_or_key Character string. An address, private key, or public key.
#'
#' @return A list with information about the input, including:
#'   \item{isvalid}{Logical indicating whether the input is valid.}
#'   \item{address}{The canonical address (if valid).}
#'   \item{ismine}{Logical indicating whether the address belongs to the node.}
#'   \item{...}{Other fields depending on the input type (e.g., pubkey, script).}
#'
#' @examples
#' \dontrun{
#' # Validate an address
#' info <- mc_validate_address(conn, "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa")
#' print(info$isvalid)
#'
#' # Validate a private key (if known)
#' key_info <- mc_validate_address(conn, "L5...")
#' }
#'
#' @seealso \code{\link{mc_import_address}} to add an address to the wallet.
#'
#' @family address operations
#' @export
mc_validate_address <- function(conn, address_or_key) {
  mc_rpc(conn, "validateaddress", list(address_or_key))
}
