#' Subscribe to assets or streams
#' 
#' Instructs the node to start tracking one or more asset(s) or stream(s).
#' 
#' @param conn Connection object.
#' @param entities A single name/ref/txid or a vector of them.
#' @param rescan If TRUE (default), the node reindexes all items from the creation of the entities.
#' @return NULL on success.
#' @export
mc_subscribe <- function(conn, entities, rescan = TRUE) {
  # Если передано несколько сущностей (вектор), превращаем в список для JSON-RPC
  entities_param <- if (length(entities) > 1) as.list(entities) else entities
  
  mc_rpc(conn, "subscribe", list(entities_param, rescan))
}

#' Unsubscribe from assets or streams
#' 
#' Instructs the node to stop tracking one or more asset(s) or stream(s).
#' 
#' @param conn Connection object.
#' @param entities A single name/ref/txid or a vector of them.
#' @param purge If TRUE (default FALSE), any off-chain data retrieved for this stream will be purged.
#' @return NULL if successful.
#' @export
mc_unsubscribe <- function(conn, entities, purge = FALSE) {
  entities_param <- if (length(entities) > 1) as.list(entities) else entities
  
  mc_rpc(conn, "unsubscribe", list(entities_param, purge))
}