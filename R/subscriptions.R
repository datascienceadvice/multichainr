#' Subscribe to MultiChain assets or streams
#' 
#' Instructs the MultiChain node to start tracking one or more asset(s) or stream(s).
#' This is often required before you can retrieve items or balances for specific entities.
#' 
#' @param conn A connection object to the MultiChain node (typically created via \code{mc_connect}).
#' @param entities A character string or vector of strings representing asset/stream names, 
#'   references, or transaction IDs (txids).
#' @param rescan Logical. If \code{TRUE} (default), the node reindexes all items from the 
#'   point of creation of the entities. If \code{FALSE}, only new items will be tracked.
#' 
#' @return Returns \code{NULL} invisibly on success, or an error if the RPC call fails.
#' 
#' @family MultiChain Entity Management
#' @seealso \code{\link{mc_unsubscribe}}
#' 
#' @export
mc_subscribe <- function(conn, entities, rescan = TRUE) {
  # If multiple entities are passed (vector), convert to list for JSON-RPC serialization
  entities_param <- if (length(entities) > 1) as.list(entities) else entities
  
  mc_rpc(conn, "subscribe", list(entities_param, rescan))
}

#' Unsubscribe from MultiChain assets or streams
#' 
#' Instructs the MultiChain node to stop tracking one or more asset(s) or stream(s).
#' 
#' @param conn A connection object to the MultiChain node.
#' @param entities A character string or vector of strings representing asset/stream names, 
#'   references, or transaction IDs (txids).
#' @param purge Logical. If \code{TRUE}, any off-chain data retrieved for this stream 
#'   will be permanently purged from the node's local storage. Defaults to \code{FALSE}.
#' 
#' @return Returns \code{NULL} invisibly on success, or an error if the RPC call fails.
#' 
#' @family MultiChain Entity Management
#' @seealso \code{\link{mc_subscribe}}
#' 
#' @export
mc_unsubscribe <- function(conn, entities, purge = FALSE) {
  # If multiple entities are passed (vector), convert to list for JSON-RPC serialization
  entities_param <- if (length(entities) > 1) as.list(entities) else entities
  
  mc_rpc(conn, "unsubscribe", list(entities_param, purge))
}
