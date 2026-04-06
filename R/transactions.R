#' Get asset balances for a specific address
#' 
#' Returns a list of all asset balances for a given address in the node's wallet.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param address Character. The MultiChain address to query.
#' @param minconf Integer. The minimum number of confirmations (default \code{1}).
#' @param include_locked Logical. If \code{TRUE}, includes unspent outputs that have been locked (default \code{FALSE}).
#' 
#' @return A data frame containing asset names, amounts, and other balance details.
#' @family transactions
#' @export
mc_get_address_balances <- function(conn, address, minconf = 1, include_locked = FALSE) {
  res <- mc_rpc(conn, "getaddressbalances", list(address, as.integer(minconf), include_locked))
  rpc_res_to_df(res)
}

#' Get details of a transaction for a specific address
#' 
#' Provides information about a specific transaction, but only if it involves the specified address.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param address Character. The MultiChain address to query.
#' @param txid Character. The transaction ID.
#' @param verbose Logical. If \code{TRUE}, provides a detailed breakdown of inputs and outputs (default \code{FALSE}).
#' 
#' @return A list containing transaction details.
#' @family transactions
#' @export
mc_get_address_transaction <- function(conn, address, txid, verbose = FALSE) {
  mc_rpc(conn, "getaddresstransaction", list(address, txid, verbose))
}

#' Get balances for multiple addresses and assets
#' 
#' Returns a breakdown of balances across a set of addresses and/or assets.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param addresses A vector of addresses, or \code{"*"} for all addresses in the wallet (default \code{"*"}).
#' @param assets A vector of asset names/refs, or \code{"*"} for all assets (default \code{"*"}).
#' @param minconf Integer. Minimum confirmations (default \code{1}).
#' @param include_watch_only Logical. Include watch-only addresses (default \code{FALSE}).
#' @param include_locked Logical. Include locked unspent outputs (default \code{FALSE}).
#' 
#' @return A list or data frame of balances, indexed by address and asset.
#' @family transactions
#' @export
mc_get_multi_balances <- function(conn, addresses = "*", assets = "*", minconf = 1, 
                                  include_watch_only = FALSE, include_locked = FALSE) {
  
  addr_param <- if (identical(addresses, "*")) "*" else as.list(addresses)
  asset_param <- if (identical(assets, "*")) "*" else as.list(assets)
  
  params <- list(
    addr_param, 
    asset_param, 
    as.integer(minconf), 
    include_watch_only, 
    include_locked
  )
  
  mc_rpc(conn, "getmultibalances", params)
}

#' Get balances for non-fungible tokens (NFTs)
#' 
#' Specifically retrieves balances for tokens (sub-assets or individual units) 
#' associated with a parent asset.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param addresses A vector of addresses, or \code{"*"} (default \code{"*"}).
#' @param assets A vector of parent asset names/refs, or \code{"*"} (default \code{"*"}).
#' @param minconf Integer. Minimum confirmations (default \code{1}).
#' @param include_watch_only Logical. Include watch-only addresses (default \code{FALSE}).
#' @param include_locked Logical. Include locked outputs (default \code{FALSE}).
#' 
#' @return A data frame containing token-level balance details.
#' @family transactions
#' @export
mc_get_token_balances <- function(conn, addresses = "*", assets = "*", minconf = 1, 
                                  include_watch_only = FALSE, include_locked = FALSE) {
  addr_param <- if (identical(addresses, "*")) "*" else as.list(addresses)
  asset_param <- if (identical(assets, "*")) "*" else as.list(assets)
  
  params <- list(
    addr_param, 
    asset_param, 
    as.integer(minconf), 
    include_watch_only, 
    include_locked
  )
  
  res <- mc_rpc(conn, "gettokenbalances", params)
  
  if (is.null(res) || length(res) == 0) return(data.frame())
  
  df_list <- lapply(names(res), function(addr) {
    item_df <- rpc_res_to_df(res[[addr]])
    if (nrow(item_df) > 0) {
      item_df$address <- addr
    }
    return(item_df)
  })
  
  final_df <- do.call(rbind, df_list)
  
  if (nrow(final_df) > 0 && "address" %in% names(final_df)) {
    cols <- c("address", setdiff(names(final_df), "address"))
    final_df <- final_df[, cols, drop = FALSE]
  }
  
  rownames(final_df) <- NULL
  return(final_df)
}

#' Get total wallet balances
#' 
#' Returns the total balance of all assets across all addresses in the wallet.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param minconf Integer. Minimum confirmations (default \code{1}).
#' @param include_watch_only Logical. Include watch-only addresses (default \code{FALSE}).
#' @param include_locked Logical. Include locked outputs (default \code{FALSE}).
#' 
#' @return A data frame summarizing total balances for each asset.
#' @family transactions
#' @export
mc_get_total_balances <- function(conn, minconf = 1, include_watch_only = FALSE, include_locked = FALSE) {
  res <- mc_rpc(conn, "gettotalbalances", list(as.integer(minconf), include_watch_only, include_locked))
  rpc_res_to_df(res)
}

#' Get details of a wallet transaction
#' 
#' Retrieves detailed information about a transaction in the node's wallet.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param txid Character. The transaction ID.
#' @param include_watch_only Logical. Include watch-only addresses (default \code{FALSE}).
#' @param verbose Logical. If \code{TRUE}, provides details of inputs and outputs (default \code{FALSE}).
#' 
#' @return A list containing detailed transaction data.
#' @family transactions
#' @export
mc_get_wallet_transaction <- function(conn, txid, include_watch_only = FALSE, verbose = FALSE) {
  mc_rpc(conn, "getwallettransaction", list(txid, include_watch_only, verbose))
}

#' List transactions for a specific address
#' 
#' Returns a list of the most recent transactions involving the specified address.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param address Character. The MultiChain address to query.
#' @param count Integer. The number of transactions to return (default \code{10}).
#' @param skip Integer. The number of transactions to skip (default \code{0}).
#' @param verbose Logical. If \code{TRUE}, provides details of inputs and outputs (default \code{FALSE}).
#' 
#' @return A data frame of transaction history for the address.
#' @family transactions
#' @export
mc_list_address_transactions <- function(conn, address, count = 10, skip = 0, verbose = FALSE) {
  res <- mc_rpc(conn, "listaddresstransactions", list(address, as.integer(count), as.integer(skip), verbose))
  rpc_res_to_df(res)
}

#' List transactions in the wallet
#' 
#' Returns a list of the most recent transactions in the wallet.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param count Integer. The number of transactions to return (default \code{10}).
#' @param skip Integer. The number of transactions to skip (default \code{0}).
#' @param include_watch_only Logical. Include watch-only addresses (default \code{FALSE}).
#' @param verbose Logical. If \code{TRUE}, provides details of inputs and outputs (default \code{FALSE}).
#' 
#' @return A data frame of transaction history for the wallet.
#' @family transactions
#' @export
mc_list_wallet_transactions <- function(conn, count = 10, skip = 0, include_watch_only = FALSE, verbose = FALSE) {
  res <- mc_rpc(conn, "listwallettransactions", list(as.integer(count), as.integer(skip), include_watch_only, verbose))
  rpc_res_to_df(res)
}

#' Retrieve full hex data from a transaction output
#' 
#' This function is used to retrieve large data payloads (like stream data) that 
#' may have been truncated in standard transaction calls.
#' 
#' @param conn A connection object to the MultiChain node.
#' @param txid Character. The transaction ID containing the output.
#' @param vout Integer. The index of the output (starting from 0).
#' @param count_bytes Integer. The number of bytes to retrieve. If \code{NULL}, returns all data.
#' @param start_byte Integer. The byte offset to start reading from (default \code{0}).
#' 
#' @return A list or string containing the hex data from the transaction output.
#' @family transactions
#' @export
mc_get_tx_out_data <- function(conn, txid, vout, count_bytes = NULL, start_byte = 0) {
  params <- list(txid, as.integer(vout))
  if (!is.null(count_bytes)) params <- c(params, list(as.integer(count_bytes), as.integer(start_byte)))
  
  mc_rpc(conn, "gettxoutdata", params)
}

#' Wait for transaction confirmation
#'
#' Blocks execution until a transaction is included in a block.
#'
#' @param conn A connection object.
#' @param txid Character string. Transaction ID.
#' @param timeout Integer. Maximum time to wait in seconds (default 30).
#'
#' @return Logical TRUE if confirmed, throws error if timeout reached.
#' @export
mc_wait_for_confirmation <- function(conn, txid, timeout = 30) {
  start_time <- Sys.time()
  while (as.numeric(difftime(Sys.time(), start_time, units = "secs")) < timeout) {
    tx_info <- tryCatch(mc_get_wallet_transaction(conn, txid), error = function(e) NULL)
    
    if (!is.null(tx_info) && !is.null(tx_info$confirmations) && tx_info$confirmations > 0) {
      return(TRUE)
    }
    Sys.sleep(1)
  }
  stop(sprintf("Timeout waiting for transaction %s to be confirmed after %s seconds", txid, timeout))
}
