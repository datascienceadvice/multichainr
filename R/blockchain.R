#' Get block information
#' 
#' @param conn Connection object.
#' @param hash_or_height Block hash or height.
#' @param verbose Verbosity level (0 to 4). Default 1.
#' @export
mc_get_block <- function(conn, hash_or_height, verbose = 1) {
  # hash_or_height может быть числом или строкой
  arg <- if (is.numeric(hash_or_height)) as.integer(hash_or_height) else hash_or_height
  mc_rpc(conn, "getblock", list(arg, verbose))
}

#' Get general blockchain information
#' @export
mc_get_blockchain_info <- function(conn) {
  mc_rpc(conn, "getblockchaininfo")
}

#' Get block hash by height
#' @export
mc_get_block_hash <- function(conn, height) {
  mc_rpc(conn, "getblockhash", list(as.integer(height)))
}

#' Get counts of blockchain entities
#' @export
mc_get_chain_totals <- function(conn) {
  mc_rpc(conn, "getchaintotals")
}

#' Get information about the last block
#' @param skip Number of blocks to skip back (default 0).
#' @export
mc_get_last_block_info <- function(conn, skip = 0) {
  mc_rpc(conn, "getlastblockinfo", list(as.integer(skip)))
}

#' Get memory pool information
#' @export
mc_get_mempool_info <- function(conn) {
  mc_rpc(conn, "getmempoolinfo")
}

#' Get list of transaction IDs in mempool
#' @export
mc_get_raw_mempool <- function(conn) {
  res <- mc_rpc(conn, "getrawmempool")
  as.character(unlist(res))
}

#' Get a raw transaction from the blockchain
#' @param txid Transaction ID.
#' @param verbose If TRUE, returns a detailed list.
#' @export
mc_get_raw_transaction <- function(conn, txid, verbose = FALSE) {
  mc_rpc(conn, "getrawtransaction", list(txid, verbose))
}

#' Get details about an unspent transaction output
#' @param txid Transaction ID.
#' @param vout Output index.
#' @param unconfirmed Include unconfirmed transactions.
#' @export
mc_get_tx_out <- function(conn, txid, vout, unconfirmed = FALSE) {
  mc_rpc(conn, "gettxout", list(txid, as.integer(vout), unconfirmed))
}

#' List information about specific blocks
#' @param blocks Range (e.g. "100-200"), hash, height, or -n for most recent.
#' @param verbose If TRUE, returns detailed information.
#' @export
mc_list_blocks <- function(conn, blocks, verbose = FALSE) {
  res <- mc_rpc(conn, "listblocks", list(blocks, verbose))
  rpc_res_to_df(res)
}

#' List miners and their status
#' @export
mc_list_miners <- function(conn, verbose = FALSE) {
  res <- mc_rpc(conn, "listminers", list(verbose))
  rpc_res_to_df(res)
}