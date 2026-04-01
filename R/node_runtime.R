#' Clear the node's memory pool
#'
#' Removes all unconfirmed transactions from the node's memory pool (mempool).
#' This function is typically used after pausing incoming and mining tasks to
#' reset the mempool state. It requires the node to be paused first with
#' \code{mc_pause(conn, "incoming,mining")}.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return Invisibly returns \code{NULL} on success.
#'
#' @examples
#' \dontrun{
#' # Pause the node before clearing the mempool
#' mc_pause(conn, "incoming,mining")
#' mc_clear_mempool(conn)
#' mc_resume(conn, "incoming,mining")
#' }
#'
#' @seealso \code{\link{mc_pause}}, \code{\link{mc_resume}},
#'   \code{\link{mc_get_mempool_info}} to inspect mempool state.
#'
#' @family mempool management
#' @export
mc_clear_mempool <- function(conn) {
  mc_rpc(conn, "clearmempool")
}

#' Get information about off-chain chunk queue
#'
#' Returns details about the node's off‑chain chunk queue, which handles
#' the transmission of large data items that are split into chunks.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return A list containing:
#'   \item{chunk_count}{Number of chunks currently queued.}
#'   \item{byte_count}{Total size in bytes of queued chunks.}
#'   \item{...}{Other queue statistics.}
#'
#' @examples
#' \dontrun{
#' queue_info <- mc_get_chunk_queue_info(conn)
#' cat("Chunks pending:", queue_info$chunk_count)
#' }
#'
#' @seealso \code{\link{mc_get_chunk_queue_totals}} for cumulative statistics.
#'
#' @family off-chain data
#' @export
mc_get_chunk_queue_info <- function(conn) {
  mc_rpc(conn, "getchunkqueueinfo")
}

#' Get cumulative statistics on off-chain chunk requests
#'
#' Returns total counts of chunk deliveries, failures, and timeouts
#' since the node started.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return A list with cumulative statistics:
#'   \item{delivered}{Number of chunks successfully delivered.}
#'   \item{undelivered}{Number of chunks not yet delivered.}
#'   \item{timeouts}{Number of delivery timeouts.}
#'   \item{...}{Other totals.}
#'
#' @examples
#' \dontrun{
#' totals <- mc_get_chunk_queue_totals(conn)
#' print(totals)
#' }
#'
#' @seealso \code{\link{mc_get_chunk_queue_info}} for current queue state.
#'
#' @family off-chain data
#' @export
mc_get_chunk_queue_totals <- function(conn) {
  mc_rpc(conn, "getchunkqueuetotals")
}

#' Pause specified node tasks
#'
#' Temporarily suspends certain node operations without shutting down the node.
#' Tasks that can be paused include mining, incoming connections, and off‑chain
#' data handling.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param tasks A character vector or a comma‑separated string of tasks to pause.
#'   Valid task names are:
#'   \itemize{
#'     \item \code{"mining"} – stop mining new blocks.
#'     \item \code{"incoming"} – stop accepting incoming connections.
#'     \item \code{"offchain"} – stop processing off‑chain data.
#'   }
#'   Multiple tasks can be specified, e.g., \code{c("mining", "incoming")}.
#'
#' @return Invisibly returns \code{NULL} on success.
#'
#' @examples
#' \dontrun{
#' # Pause mining only
#' mc_pause(conn, "mining")
#'
#' # Pause both incoming connections and mining
#' mc_pause(conn, c("incoming", "mining"))
#' }
#'
#' @seealso \code{\link{mc_resume}} to restart paused tasks,
#'   \code{\link{mc_clear_mempool}} for use after pausing.
#'
#' @family node control
#' @export
mc_pause <- function(conn, tasks) {
  if (length(tasks) > 1) tasks <- paste(tasks, collapse = ",")
  mc_rpc(conn, "pause", list(tasks))
}

#' Resume specified node tasks
#'
#' Restarts node tasks that were previously paused with \code{\link{mc_pause}}.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param tasks A character vector or a comma‑separated string of tasks to resume.
#'   Valid task names: \code{"mining"}, \code{"incoming"}, \code{"offchain"}.
#'
#' @return Invisibly returns \code{NULL} on success.
#'
#' @examples
#' \dontrun{
#' # Resume mining after a pause
#' mc_resume(conn, "mining")
#'
#' # Resume all paused tasks
#' mc_resume(conn, c("mining", "incoming", "offchain"))
#' }
#'
#' @seealso \code{\link{mc_pause}} to pause tasks.
#'
#' @family node control
#' @export
mc_resume <- function(conn, tasks) {
  if (length(tasks) > 1) tasks <- paste(tasks, collapse = ",")
  mc_rpc(conn, "resume", list(tasks))
}

#' Rewind the node's active chain
#'
#' Moves the node's active chain to a previous block, effectively
#' rolling back the blockchain state. This is a powerful operation
#' typically used for testing or recovery. The node must be paused
#' with \code{mc_pause(conn, "incoming,mining")} before calling.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param hash_or_height Either a block hash (character string) or a
#'   block height (integer) to rewind to.
#'
#' @return Character string. The hash of the last block after the rewind.
#'
#' @examples
#' \dontrun{
#' # Pause the node first
#' mc_pause(conn, "incoming,mining")
#' # Rewind to block height 100
#' last_hash <- mc_set_last_block(conn, 100)
#' # Resume after rewind
#' mc_resume(conn, "incoming,mining")
#' }
#'
#' @seealso \code{\link{mc_pause}}, \code{\link{mc_resume}},
#'   \code{\link{mc_get_block}} to inspect blocks.
#'
#' @family node control
#' @export
mc_set_last_block <- function(conn, hash_or_height) {
  arg <- if (is.numeric(hash_or_height)) as.integer(hash_or_height) else hash_or_height
  mc_rpc(conn, "setlastblock", list(arg))
}
