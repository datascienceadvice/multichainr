#' Combine unspent outputs (UTXOs)
#' 
#' Sends transactions to combine many small unspent transaction outputs (UTXOs) 
#' into a single output. This is used to improve wallet performance and reduce 
#' the size of the wallet's UTXO set.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param addresses A vector of addresses, or \code{"*"} for all addresses (default \code{"*"}).
#' @param minconf Integer. Minimum confirmations (default \code{1}).
#' @param maxcombines Integer. Maximum number of transactions to create (default \code{100}).
#' @param mininputs Integer. Minimum number of inputs per transaction (default \code{2}).
#' @param maxinputs Integer. Maximum number of inputs per transaction (default \code{100}).
#' @param maxtime Integer. Maximum seconds to spend combining (default \code{15}).
#' 
#' @return A character vector of the transaction IDs (txids) created.
#' @family wallet
#' @export
mc_combine_unspent <- function(conn, addresses = "*", minconf = 1, maxcombines = 100, 
                               mininputs = 2, maxinputs = 100, maxtime = 15) {
  params <- list(
    if (length(addresses) > 1) as.list(addresses) else addresses,
    as.integer(minconf),
    as.integer(maxcombines),
    as.integer(mininputs),
    as.integer(maxinputs),
    as.integer(maxtime)
  )
  res <- mc_rpc(conn, "combineunspent", params)
  as.character(unlist(res))
}

#' List locked unspent outputs
#' 
#' Returns a list of unspent transaction outputs that have been temporarily 
#' locked by \code{\link{mc_lock_unspent}}.
#' 
#' @param conn A connection object to the MultiChain node.
#' 
#' @return A data frame containing columns for \code{txid} and \code{vout}.
#' @family wallet
#' @export
mc_list_lock_unspent <- function(conn) {
  res <- mc_rpc(conn, "listlockunspent")
  rpc_res_to_df(res)
}

#' List unspent transaction outputs
#' 
#' Returns a list of all unspent outputs (UTXOs) available in the wallet.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param minconf Integer. Minimum confirmations (default \code{1}).
#' @param maxconf Integer. Maximum confirmations (default \code{999999}).
#' @param addresses Optional character vector of addresses to filter the results.
#' 
#' @return A data frame containing UTXO details, including \code{txid}, \code{vout}, 
#'   \code{address}, \code{amount}, and associated asset/permission data.
#' @family wallet
#' @export
mc_list_unspent <- function(conn, minconf = 1, maxconf = 999999, addresses = NULL) {
  params <- list(as.integer(minconf), as.integer(maxconf))
  if (!is.null(addresses)) params <- c(params, list(as.list(addresses)))
  
  res <- mc_rpc(conn, "listunspent", params)
  rpc_res_to_df(res)
}

#' Lock or unlock unspent outputs
#' 
#' Prevents specific UTXOs from being spent in automated transactions, or 
#' releases them if they were previously locked.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param unlock Logical. If \code{TRUE}, unlocks the outputs; if \code{FALSE}, locks them.
#' @param outputs Optional list of outputs to (un)lock. Expected format: 
#'   \code{list(list(txid="id", vout=0), ...)}. If empty and \code{unlock=TRUE}, 
#'   all outputs are unlocked.
#' 
#' @return Logical \code{TRUE} on success.
#' @family wallet
#' @export
mc_lock_unspent <- function(conn, unlock, outputs = NULL) {
  params <- list(as.logical(unlock))
  if (!is.null(outputs)) params <- c(params, list(outputs))
  
  as.logical(mc_rpc(conn, "lockunspent", params))
}

#' Backup the wallet file
#' 
#' Safely copies the \code{wallet.dat} file to a specified destination.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param filename Character. The full path and filename for the backup. 
#'   \strong{Note:} This path is relative to the machine where the MultiChain node is running.
#' 
#' @return Returns \code{NULL} on success.
#' @family wallet
#' @export
mc_backup_wallet <- function(conn, filename) {
  mc_rpc(conn, "backupwallet", list(filename))
}

#' Dump a private key for an address
#' 
#' @param conn A connection object to the MultiChain node.
#' @param address Character. The wallet address for which to retrieve the private key.
#' 
#' @return Character. The private key in Wallet Import Format (WIF).
#' @family wallet
#' @export
mc_dump_privkey <- function(conn, address) {
  mc_rpc(conn, "dumpprivkey", list(address))
}

#' Dump all private keys to a file
#' 
#' Exports all wallet private keys into a human-readable text file.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param filename Character. Full path for the text file on the node's machine.
#' 
#' @return Returns \code{NULL} on success.
#' @family wallet
#' @export
mc_dump_wallet <- function(conn, filename) {
  mc_rpc(conn, "dumpwallet", list(filename))
}

#' Encrypt the wallet
#' 
#' Encrypts the wallet with a passphrase for the first time.
#' 
#' @section Warning:
#' MultiChain will shut down after this command is successful. You must 
#' manually restart the node to continue operations.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param passphrase Character. The new wallet password.
#' 
#' @return Returns a message indicating the node is shutting down.
#' @family wallet
#' @export
mc_encrypt_wallet <- function(conn, passphrase) {
  mc_rpc(conn, "encryptwallet", list(passphrase))
}

#' Get general wallet information
#' 
#' Returns metadata about the wallet, such as version, balance, and encryption status.
#' 
#' @param conn A connection object to the MultiChain node.
#' 
#' @return A list of wallet information.
#' @family wallet
#' @export
mc_get_wallet_info <- function(conn) {
  mc_rpc(conn, "getwalletinfo")
}

#' Import one or more private keys
#' 
#' Adds private keys to the node's wallet.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param privkeys A character string or vector of private keys (WIF format).
#' @param label Optional character string. A label to assign to the addresses.
#' @param rescan Logical or Integer. If \code{TRUE}, rescans the entire blockchain. 
#'   Can also be an integer representing the block height to start scanning from.
#' 
#' @return Returns \code{NULL} on success.
#' @family wallet
#' @export
mc_import_privkey <- function(conn, privkeys, label = "", rescan = TRUE) {
  keys_param <- if (length(privkeys) > 1) as.list(privkeys) else privkeys
  mc_rpc(conn, "importprivkey", list(keys_param, label, rescan))
}

#' Import keys from a wallet dump file
#' 
#' @param conn A connection object to the MultiChain node.
#' @param filename Character. Path to the dump file on the node's machine.
#' @param rescan Integer. The block number to start rescanning from (default \code{0}).
#' 
#' @return Returns \code{NULL} on success.
#' @family wallet
#' @export
mc_import_wallet <- function(conn, filename, rescan = 0) {
  mc_rpc(conn, "importwallet", list(filename, as.integer(rescan)))
}

#' Immediately lock the wallet
#' 
#' Removes the wallet encryption key from memory, requiring a passphrase for 
#' further private key operations.
#' 
#' @param conn A connection object to the MultiChain node.
#' 
#' @return Returns \code{NULL} on success.
#' @family wallet
#' @export
mc_lock_wallet <- function(conn) {
  mc_rpc(conn, "walletlock")
}

#' Unlock the wallet with a passphrase
#' 
#' Stores the wallet decryption key in memory for a specified duration.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param passphrase Character. The wallet password.
#' @param timeout Integer. Time in seconds to keep the wallet unlocked.
#' 
#' @return Returns \code{NULL} on success.
#' @family wallet
#' @export
mc_unlock_wallet <- function(conn, passphrase, timeout) {
  mc_rpc(conn, "walletpassphrase", list(passphrase, as.integer(timeout)))
}

#' Change the wallet passphrase
#' 
#' Changes the encryption password of the wallet.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param old_passphrase Character. The current password.
#' @param new_passphrase Character. The new password.
#' 
#' @return Returns \code{NULL} on success.
#' @family wallet
#' @export
mc_change_wallet_passphrase <- function(conn, old_passphrase, new_passphrase) {
  mc_rpc(conn, "walletpassphrasechange", list(old_passphrase, new_passphrase))
}
