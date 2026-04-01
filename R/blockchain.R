#' Get block information
#'
#' Retrieves detailed information about a specific block. The block can be
#' identified either by its hash (string) or height (integer). The verbosity
#' level controls how much detail is returned.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param hash_or_height Either a character string (block hash) or an integer
#'   (block height).
#' @param verbose Integer. Verbosity level from 0 to 4. Default is \code{1}.
#'   * \code{0}: returns only the block hash as a string.
#'   * \code{1}: returns a list with basic block information.
#'   * \code{2–4}: include additional details (transactions, etc.).
#'
#' @return Depends on \code{verbose}:
#'   * If \code{verbose = 0}: a character string with the block hash.
#'   * If \code{verbose >= 1}: a list containing block details (height, time,
#'     transactions, etc.).
#'
#' @examples
#' \dontrun{
#' # Get block by height
#' block <- mc_get_block(conn, 123456)
#'
#' # Get block by hash with full transaction details
#' block <- mc_get_block(conn, "0000...", verbose = 2)
#' }
#'
#' @seealso \code{\link{mc_get_block_hash}} to obtain a block hash from height,
#'   \code{\link{mc_list_blocks}} to list multiple blocks.
#'
#' @family blockchain information
#' @export
mc_get_block <- function(conn, hash_or_height, verbose = 1) {
  # hash_or_height может быть числом или строкой
  arg <- if (is.numeric(hash_or_height)) as.integer(hash_or_height) else hash_or_height
  mc_rpc(conn, "getblock", list(arg, verbose))
}

#' Get general blockchain information
#'
#' Returns global information about the blockchain, such as the current block
#' height, chain name, protocol version, difficulty, and consensus status.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return A list with blockchain metadata. Typical fields:
#'   \item{chain}{Name of the chain.}
#'   \item{blocks}{Current block height.}
#'   \item{headers}{Number of block headers.}
#'   \item{bestblockhash}{Hash of the most recent block.}
#'   \item{difficulty}{Current mining difficulty.}
#'   \item{chainwork}{Total work in the chain.}
#'
#' @examples
#' \dontrun{
#' info <- mc_get_blockchain_info(conn)
#' print(info$blocks)
#' }
#'
#' @seealso \code{\link{mc_get_chain_totals}} for counts of entities,
#'   \code{\link{mc_get_last_block_info}} for the most recent block.
#'
#' @family blockchain information
#' @export
mc_get_blockchain_info <- function(conn) {
  mc_rpc(conn, "getblockchaininfo")
}

#' Get block hash by height
#'
#' Returns the hash of a block at a given height.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param height Integer. The block height (0 for genesis block).
#'
#' @return A character string containing the block hash.
#'
#' @examples
#' \dontrun{
#' hash <- mc_get_block_hash(conn, 0)  # genesis block hash
#' }
#'
#' @seealso \code{\link{mc_get_block}} to retrieve block details.
#'
#' @family blockchain information
#' @export
mc_get_block_hash <- function(conn, height) {
  mc_rpc(conn, "getblockhash", list(as.integer(height)))
}

#' Get counts of blockchain entities
#'
#' Returns the total number of various objects in the blockchain, such as
#' addresses, transactions, assets, streams, and permissions.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return A list with counts, typically containing:
#'   \item{addresses}{Number of addresses.}
#'   \item{transactions}{Number of transactions.}
#'   \item{assets}{Number of assets.}
#'   \item{streams}{Number of streams.}
#'   \item{permissions}{Number of permission entries.}
#'
#' @examples
#' \dontrun{
#' totals <- mc_get_chain_totals(conn)
#' print(totals$transactions)
#' }
#'
#' @seealso \code{\link{mc_get_blockchain_info}} for global blockchain stats.
#'
#' @family blockchain information
#' @export
mc_get_chain_totals <- function(conn) {
  mc_rpc(conn, "getchaintotals")
}

#' Get information about the last block
#'
#' Retrieves details of the most recent block, optionally skipping back
#' by a number of blocks.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param skip Integer. Number of blocks to skip back from the tip.
#'   \code{skip = 0} returns the latest block, \code{skip = 1} returns the
#'   previous block, etc. Default is \code{0}.
#'
#' @return A list with block information (similar to \code{\link{mc_get_block}}).
#'
#' @examples
#' \dontrun{
#' latest <- mc_get_last_block_info(conn)
#' previous <- mc_get_last_block_info(conn, skip = 1)
#' }
#'
#' @seealso \code{\link{mc_get_block}} for general block retrieval.
#'
#' @family blockchain information
#' @export
mc_get_last_block_info <- function(conn, skip = 0) {
  mc_rpc(conn, "getlastblockinfo", list(as.integer(skip)))
}

#' Get memory pool information
#'
#' Returns information about the node's memory pool (mempool), which holds
#' unconfirmed transactions awaiting inclusion in a block.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return A list with mempool statistics:
#'   \item{size}{Number of transactions in the mempool.}
#'   \item{bytes}{Total size in bytes.}
#'   \item{usage}{Memory usage.}
#'
#' @examples
#' \dontrun{
#' mempool <- mc_get_mempool_info(conn)
#' print(paste("Pending transactions:", mempool$size))
#' }
#'
#' @seealso \code{\link{mc_get_raw_mempool}} for the list of transaction IDs.
#'
#' @family mempool & transactions
#' @export
mc_get_mempool_info <- function(conn) {
  mc_rpc(conn, "getmempoolinfo")
}

#' Get list of transaction IDs in mempool
#'
#' Returns a character vector of transaction IDs currently in the node's memory pool.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return A character vector of transaction IDs (txids).
#'
#' @examples
#' \dontrun{
#' pending <- mc_get_raw_mempool(conn)
#' length(pending)  # number of pending transactions
#' }
#'
#' @seealso \code{\link{mc_get_mempool_info}} for mempool statistics.
#'
#' @family mempool & transactions
#' @export
mc_get_raw_mempool <- function(conn) {
  res <- mc_rpc(conn, "getrawmempool")
  as.character(unlist(res))
}

#' Get a raw transaction from the blockchain
#'
#' Retrieves a transaction from the blockchain by its transaction ID. If
#' \code{verbose = TRUE}, returns a detailed decoded transaction; if
#' \code{FALSE} (default), returns the raw transaction in hexadecimal.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param txid Character string. Transaction ID.
#' @param verbose Logical. If \code{TRUE}, returns a parsed transaction object;
#'   if \code{FALSE}, returns the raw transaction hex. Default is \code{FALSE}.
#'
#' @return If \code{verbose = FALSE}, a character string (hex). If
#'   \code{verbose = TRUE}, a list with transaction details.
#'
#' @examples
#' \dontrun{
#' # Get raw hex of a transaction
#' raw <- mc_get_raw_transaction(conn, "abc...")
#'
#' # Get decoded transaction
#' tx <- mc_get_raw_transaction(conn, "abc...", verbose = TRUE)
#' }
#'
#' @seealso \code{\link{mc_get_tx_out}} to inspect a specific output.
#'
#' @family mempool & transactions
#' @export
mc_get_raw_transaction <- function(conn, txid, verbose = FALSE) {
  mc_rpc(conn, "getrawtransaction", list(txid, verbose))
}

#' Get details about an unspent transaction output
#'
#' Returns information about a specific unspent transaction output (UTXO).
#' This can be useful for building transactions or checking balances.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param txid Character string. Transaction ID.
#' @param vout Integer. Output index.
#' @param unconfirmed Logical. If \code{TRUE}, includes unconfirmed
#'   transactions (from the mempool). Default is \code{FALSE}.
#'
#' @return A list with output details, including:
#'   \item{bestblock}{Hash of the best block.}
#'   \item{confirmations}{Number of confirmations.}
#'   \item{value}{Amount (in native currency).}
#'   \item{scriptPubKey}{Output script details.}
#'   Returns \code{NULL} if the output does not exist or is spent.
#'
#' @examples
#' \dontrun{
#' # Check an output from a confirmed transaction
#' out <- mc_get_tx_out(conn, "abc...", vout = 0)
#' if (!is.null(out)) print(out$value)
#' }
#'
#' @seealso \code{\link{mc_get_raw_transaction}} for full transaction details.
#'
#' @family mempool & transactions
#' @export
mc_get_tx_out <- function(conn, txid, vout, unconfirmed = FALSE) {
  mc_rpc(conn, "gettxout", list(txid, as.integer(vout), unconfirmed))
}

#' List information about specific blocks
#'
#' Retrieves information about one or more blocks. The \code{blocks} parameter
#' can be a single block height/hash, a range (e.g., \code{"100-200"}),
#' or \code{-\%d} to list the most recent blocks.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param blocks Specification of which blocks to list. Can be:
#'   * a single block height (integer) or hash (string),
#'   * a range string like \code{"100-200"},
#'   * a negative integer \code{-n} to list the \code{n} most recent blocks.
#' @param verbose Logical. If \code{TRUE}, returns detailed block information.
#'   Default is \code{FALSE}.
#'
#' @return A data frame (converted via \code{rpc_res_to_df}) with one row per block.
#'
#' @examples
#' \dontrun{
#' # List the last 5 blocks
#' last5 <- mc_list_blocks(conn, -5)
#'
#' # List blocks 100 to 105
#' range <- mc_list_blocks(conn, "100-105", verbose = TRUE)
#' }
#'
#' @seealso \code{\link{mc_get_block}} for a single block.
#'
#' @family blockchain information
#' @export
mc_list_blocks <- function(conn, blocks, verbose = FALSE) {
  res <- mc_rpc(conn, "listblocks", list(blocks, verbose))
  rpc_res_to_df(res)
}

#' List miners and their status
#'
#' Returns information about nodes that are mining (or have mining permission)
#' on the blockchain.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param verbose Logical. If \code{TRUE}, returns additional details about
#'   each miner (e.g., mining status, last block mined). Default is \code{FALSE}.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with miner information.
#'   Typical columns: \code{address}, \code{status}, \code{lastblocktime}, etc.
#'
#' @examples
#' \dontrun{
#' miners <- mc_list_miners(conn)
#' print(miners)
#' }
#'
#' @family blockchain information
#' @export
mc_list_miners <- function(conn, verbose = FALSE) {
  res <- mc_rpc(conn, "listminers", list(verbose))
  rpc_res_to_df(res)
}
