#' Add or remove a peer-to-peer connection
#' 
#' @param conn Connection object.
#' @param node IP address and port (e.g., "127.0.0.1:8571").
#' @param command One of "add" (queue), "remove" (disconnect), or "onetry" (immediate attempt).
#' @return NULL on success.
#' @export
mc_add_node <- function(conn, node, command = c("add", "remove", "onetry")) {
  command <- match.arg(command)
  mc_rpc(conn, "addnode", list(node, command))
}

#' Get information about manually added nodes
#' 
#' @param conn Connection object.
#' @param verbose If TRUE, returns detailed information.
#' @param node Optional specific node address.
#' @return A list or data frame of added nodes.
#' @export
mc_get_added_node_info <- function(conn, verbose = FALSE, node = NULL) {
  params <- list(verbose)
  if (!is.null(node)) params <- c(params, list(node))
  
  res <- mc_rpc(conn, "getaddednodeinfo", params)
  if (verbose) return(rpc_res_to_df(res))
  return(as.character(unlist(res)))
}

#' Get information about node's network status
#' 
#' @param conn Connection object.
#' @return A list containing port and local address information.
#' @export
mc_get_network_info <- function(conn) {
  mc_rpc(conn, "getnetworkinfo")
}

#' Get information about connected peers
#' 
#' @param conn Connection object.
#' @return A data frame with peer details (latency, version, IP, etc.).
#' @export
mc_get_peer_info <- function(conn) {
  res <- mc_rpc(conn, "getpeerinfo")
  rpc_res_to_df(res)
}

#' List known peer node addresses (MultiChain 2.3+)
#' 
#' @param conn Connection object.
#' @param include_old_ignores Include old ignored nodes (default FALSE).
#' @return A data frame of stored nodes.
#' @export
mc_list_stored_nodes <- function(conn, include_old_ignores = FALSE) {
  res <- mc_rpc(conn, "liststorednodes", list(include_old_ignores))
  rpc_res_to_df(res)
}

#' Ping all connected peers
#' 
#' Measures network latency. Results can be seen in mc_get_peer_info().
#' @return NULL on success.
#' @export
mc_ping <- function(conn) {
  mc_rpc(conn, "ping")
}

#' Add an IP address to known peer nodes (MultiChain 2.3+)
#' 
#' @param conn Connection object.
#' @param node IP address and port.
#' @param command One of "tryconnect" (default) or "ignore".
#' @export
mc_store_node <- function(conn, node, command = c("tryconnect", "ignore")) {
  command <- match.arg(command)
  mc_rpc(conn, "storenode", list(node, command))
}