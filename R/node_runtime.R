#' Clear the node's memory pool
#' 
#' Removes all unconfirmed transactions from the mempool.
#' Note: Must be called after mc_pause(conn, "incoming,mining").
#' @param conn Connection object.
#' @return NULL on success.
#' @export
mc_clear_mempool <- function(conn) {
  mc_rpc(conn, "clearmempool")
}

#' Get information about off-chain chunk queue
#' @param conn Connection object.
#' @return A list containing chunk counts and byte sizes.
#' @export
mc_get_chunk_queue_info <- function(conn) {
  mc_rpc(conn, "getchunkqueueinfo")
}

#' Get cumulative statistics on off-chain chunk requests
#' @param conn Connection object.
#' @return A list of statistics (delivered, undelivered, timeouts, etc.).
#' @export
mc_get_chunk_queue_totals <- function(conn) {
  mc_rpc(conn, "getchunkqueuetotals")
}

#' Pause specified node tasks
#' 
#' @param conn Connection object.
#' @param tasks A character vector or comma-separated string of tasks 
#' (mining, incoming, offchain).
#' @return NULL on success.
#' @export
mc_pause <- function(conn, tasks) {
  if (length(tasks) > 1) tasks <- paste(tasks, collapse = ",")
  mc_rpc(conn, "pause", list(tasks))
}

#' Resume specified node tasks
#' 
#' @param conn Connection object.
#' @param tasks A character vector or comma-separated string of tasks.
#' @return NULL on success.
#' @export
mc_resume <- function(conn, tasks) {
  if (length(tasks) > 1) tasks <- paste(tasks, collapse = ",")
  mc_rpc(conn, "resume", list(tasks))
}

#' Rewind the node's active chain
#' 
#' Note: Must be called after mc_pause(conn, "incoming,mining").
#' @param conn Connection object.
#' @param hash_or_height Block hash or height to rewind to.
#' @return The hash of the last block after the change.
#' @export
mc_set_last_block <- function(conn, hash_or_height) {
  arg <- if (is.numeric(hash_or_height)) as.integer(hash_or_height) else hash_or_height
  mc_rpc(conn, "setlastblock", list(arg))
}