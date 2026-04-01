#' Combine unspent outputs
#' 
#' Sends transactions to combine many small UTXOs into one to improve wallet performance.
#' 
#' @param conn Connection object.
#' @param addresses Vector of addresses or "*" (default "*").
#' @param minconf Minimum confirmations (default 1).
#' @param maxcombines Maximum transactions to create (default 100).
#' @param mininputs Minimum inputs per transaction (default 2).
#' @param maxinputs Maximum inputs per transaction (default 100).
#' @param maxtime Maximum seconds to spend combining (default 15).
#' @return A character vector of transaction IDs created.
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
#' @param conn Connection object.
#' @return A data frame of locked outputs (txid and vout).
#' @export
mc_list_lock_unspent <- function(conn) {
  res <- mc_rpc(conn, "listlockunspent")
  rpc_res_to_df(res)
}

#' List unspent transaction outputs
#' 
#' @param conn Connection object.
#' @param minconf Minimum confirmations (default 1).
#' @param maxconf Maximum confirmations (default 999999).
#' @param addresses Optional vector of addresses to filter.
#' @return A data frame containing UTXO details, including assets and permissions.
#' @export
mc_list_unspent <- function(conn, minconf = 1, maxconf = 999999, addresses = NULL) {
  params <- list(as.integer(minconf), as.integer(maxconf))
  if (!is.null(addresses)) params <- c(params, list(as.list(addresses)))
  
  res <- mc_rpc(conn, "listunspent", params)
  rpc_res_to_df(res)
}

#' Lock or unlock unspent outputs
#' 
#' @param conn Connection object.
#' @param unlock Logical. If TRUE, unlocks; if FALSE, locks.
#' @param outputs Optional list of outputs to (un)lock: list(list(txid="id", vout=0), ...).
#' @return Logical TRUE on success.
#' @export
mc_lock_unspent <- function(conn, unlock, outputs = NULL) {
  params <- list(as.logical(unlock))
  if (!is.null(outputs)) params <- c(params, list(outputs))
  
  as.logical(mc_rpc(conn, "lockunspent", params))
}

#' Backup the wallet file
#' @param conn Connection object.
#' @param filename Full path and name for the backup file on the node's machine.
#' @export
mc_backup_wallet <- function(conn, filename) {
  mc_rpc(conn, "backupwallet", list(filename))
}

#' Dump a private key for an address
#' @param conn Connection object.
#' @param address The wallet address.
#' @return The private key in WIF format.
#' @export
mc_dump_privkey <- function(conn, address) {
  mc_rpc(conn, "dumpprivkey", list(address))
}

#' Dump all private keys to a file
#' @param conn Connection object.
#' @param filename Full path for the text file on the node's machine.
#' @export
mc_dump_wallet <- function(conn, filename) {
  mc_rpc(conn, "dumpwallet", list(filename))
}

#' Encrypt the wallet for the first time
#' 
#' WARNING: MultiChain will stop after this and must be restarted.
#' @param conn Connection object.
#' @param passphrase The new wallet password.
#' @export
mc_encrypt_wallet <- function(conn, passphrase) {
  mc_rpc(conn, "encryptwallet", list(passphrase))
}

#' Get general wallet information
#' @export
mc_get_wallet_info <- function(conn) {
  mc_rpc(conn, "getwalletinfo")
}

#' Import one or more private keys
#' @param conn Connection object.
#' @param privkeys A single private key string or a vector of strings.
#' @param label Optional label for the addresses.
#' @param rescan If TRUE (default), rescans the whole chain. Can be an integer block height.
#' @export
mc_import_privkey <- function(conn, privkeys, label = "", rescan = TRUE) {
  keys_param <- if (length(privkeys) > 1) as.list(privkeys) else privkeys
  mc_rpc(conn, "importprivkey", list(keys_param, label, rescan))
}

#' Import keys from a wallet dump file
#' @param conn Connection object.
#' @param filename Path to the dump file.
#' @param rescan Block number to start rescanning from (default 0).
#' @export
mc_import_wallet <- function(conn, filename, rescan = 0) {
  mc_rpc(conn, "importwallet", list(filename, as.integer(rescan)))
}

#' Immediately lock the wallet
#' @export
mc_lock_wallet <- function(conn) {
  mc_rpc(conn, "walletlock")
}

#' Unlock the wallet with a passphrase
#' @param conn Connection object.
#' @param passphrase The wallet password.
#' @param timeout Time in seconds to keep the wallet unlocked.
#' @export
mc_unlock_wallet <- function(conn, passphrase, timeout) {
  mc_rpc(conn, "walletpassphrase", list(passphrase, as.integer(timeout)))
}

#' Change the wallet passphrase
#' @export
mc_change_wallet_passphrase <- function(conn, old_passphrase, new_passphrase) {
  mc_rpc(conn, "walletpassphrasechange", list(old_passphrase, new_passphrase))
}