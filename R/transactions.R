#' Get asset balances for an address
#' 
#' @param conn Connection object.
#' @param address Wallet address.
#' @param minconf Minimum number of confirmations (default 1).
#' @param include_locked Include unspent outputs which have been locked (default FALSE).
#' @export
mc_get_address_balances <- function(conn, address, minconf = 1, include_locked = FALSE) {
  res <- mc_rpc(conn, "getaddressbalances", list(address, as.integer(minconf), include_locked))
  rpc_res_to_df(res)
}

#' Get details of a transaction related to an address
#' 
#' @param conn Connection object.
#' @param address Wallet address.
#' @param txid Transaction ID.
#' @param verbose If TRUE, provides details of inputs and outputs.
#' @export
mc_get_address_transaction <- function(conn, address, txid, verbose = FALSE) {
  mc_rpc(conn, "getaddresstransaction", list(address, txid, verbose))
}

#' Get balances for multiple addresses and assets
#' 
#' @param conn Connection object.
#' @param addresses Vector of addresses or "*" for all (default "*").
#' @param assets Vector of asset names/refs or "*" for all (default "*").
#' @param minconf Minimum confirmations (default 1).
#' @param include_watch_only Include watch-only addresses (default FALSE).
#' @param include_locked Include locked unspent outputs (default FALSE).
#' @export
mc_get_multi_balances <- function(conn, addresses = "*", assets = "*", minconf = 1, 
                                  include_watch_only = FALSE, include_locked = FALSE) {
  params <- list(as.list(addresses), as.list(assets), as.integer(minconf), 
                 include_watch_only, include_locked)
  mc_rpc(conn, "getmultibalances", params)
}

#' Get balances for non-fungible tokens (NFTs)
#' 
#' @param conn Connection object.
#' @param addresses Vector of addresses or "*" (default "*").
#' @param assets Vector of parent asset names/refs or "*" (default "*").
#' @param minconf Minimum confirmations (default 1).
#' @param include_watch_only Include watch-only addresses (default FALSE).
#' @param include_locked Include locked outputs (default FALSE).
#' @export
mc_get_token_balances <- function(conn, addresses = "*", assets = "*", minconf = 1, 
                                  include_watch_only = FALSE, include_locked = FALSE) {
  params <- list(as.list(addresses), as.list(assets), as.integer(minconf), 
                 include_watch_only, include_locked)
  res <- mc_rpc(conn, "gettokenbalances", params)
  rpc_res_to_df(res)
}

#' Get total wallet balances
#' 
#' @param conn Connection object.
#' @param minconf Minimum confirmations (default 1).
#' @param include_watch_only Include watch-only addresses (default FALSE).
#' @param include_locked Include locked outputs (default FALSE).
#' @export
mc_get_total_balances <- function(conn, minconf = 1, include_watch_only = FALSE, include_locked = FALSE) {
  res <- mc_rpc(conn, "gettotalbalances", list(as.integer(minconf), include_watch_only, include_locked))
  rpc_res_to_df(res)
}

#' Get details of a wallet transaction
#' 
#' @param conn Connection object.
#' @param txid Transaction ID.
#' @param include_watch_only Include watch-only addresses (default FALSE).
#' @param verbose If TRUE, provides details of inputs and outputs.
#' @export
mc_get_wallet_transaction <- function(conn, txid, include_watch_only = FALSE, verbose = FALSE) {
  mc_rpc(conn, "getwallettransaction", list(txid, include_watch_only, verbose))
}

#' List transactions for an address
#' 
#' @param conn Connection object.
#' @param address Wallet address.
#' @param count Number of transactions (default 10).
#' @param skip Number of transactions to skip (default 0).
#' @param verbose If TRUE, provides details of inputs and outputs.
#' @export
mc_list_address_transactions <- function(conn, address, count = 10, skip = 0, verbose = FALSE) {
  res <- mc_rpc(conn, "listaddresstransactions", list(address, as.integer(count), as.integer(skip), verbose))
  rpc_res_to_df(res)
}

#' List transactions in the wallet
#' 
#' @param conn Connection object.
#' @param count Number of transactions (default 10).
#' @param skip Number of transactions to skip (default 0).
#' @param include_watch_only Include watch-only addresses (default FALSE).
#' @param verbose If TRUE, provides details of inputs and outputs.
#' @export
mc_list_wallet_transactions <- function(conn, count = 10, skip = 0, include_watch_only = FALSE, verbose = FALSE) {
  res <- mc_rpc(conn, "listwallettransactions", list(as.integer(count), as.integer(skip), include_watch_only, verbose))
  rpc_res_to_df(res)
}

#' Retrieve full hex data from a transaction output
#' 
#' Useful for retrieving large stream data that was truncated.
#' @export
mc_get_tx_out_data <- function(conn, txid, vout, count_bytes = NULL, start_byte = 0) {
  params <- list(txid, as.integer(vout))
  if (!is.null(count_bytes)) params <- c(params, list(as.integer(count_bytes), as.integer(start_byte)))
  
  mc_rpc(conn, "gettxoutdata", params)
}