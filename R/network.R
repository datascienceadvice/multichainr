#' Add or remove a peer-to-peer connection
#'
#' Manages the node's peer connections. Can add a node to the connection queue,
#' remove an existing connection, or attempt a one‑time connection.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param node Character string. The IP address and port of the peer node,
#'   e.g., \code{"127.0.0.1:8571"}.
#' @param command Character string. The action to perform:
#'   \itemize{
#'     \item \code{"add"} – add the node to the connection queue (will attempt to
#'           connect and stay connected).
#'     \item \code{"remove"} – disconnect from the node if currently connected.
#'     \item \code{"onetry"} – attempt a single connection; do not keep retrying.
#'   }
#'
#' @return Invisibly returns \code{NULL} on success; throws an error if the
#'   command fails.
#'
#' @examples
#' \dontrun{
#' # Add a peer
#' mc_add_node(conn, "192.168.1.10:8571", command = "add")
#'
#' # Remove a peer
#' mc_add_node(conn, "192.168.1.10:8571", command = "remove")
#' }
#'
#' @seealso \code{\link{mc_get_added_node_info}} to list added nodes,
#'   \code{\link{mc_get_peer_info}} for connected peers.
#'
#' @family networking
#' @export
mc_add_node <- function(conn, node, command = c("add", "remove", "onetry")) {
  command <- match.arg(command)
  mc_rpc(conn, "addnode", list(node, command))
}

#' Get information about manually added nodes
#'
#' Returns details about nodes that were added via \code{\link{mc_add_node}}.
#' Can return either a list of node addresses or detailed information.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param verbose Logical. If \code{TRUE}, returns a data frame with detailed
#'   information about each added node (e.g., connected status, last connection
#'   time). If \code{FALSE} (default), returns a character vector of node
#'   addresses.
#' @param node Optional character string. If provided, returns information
#'   only for that specific node.
#'
#' @return If \code{verbose = FALSE}: a character vector of node addresses.
#'   If \code{verbose = TRUE}: a data frame (via \code{rpc_res_to_df}) with
#'   node details.
#'
#' @examples
#' \dontrun{
#' # List all added nodes (addresses only)
#' nodes <- mc_get_added_node_info(conn)
#'
#' # Get detailed information for a specific node
#' details <- mc_get_added_node_info(conn, verbose = TRUE, node = "192.168.1.10:8571")
#' }
#'
#' @seealso \code{\link{mc_add_node}}, \code{\link{mc_get_peer_info}}
#'
#' @family networking
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
#' Returns details about the node's network configuration, including listening
#' port, local addresses, and network‑related flags.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return A list containing network information, typically:
#'   \item{version}{Node version.}
#'   \item{subversion}{Node subversion string.}
#'   \item{protocolversion}{Protocol version.}
#'   \item{localservices}{Services offered by the node.}
#'   \item{localaddresses}{List of local IP addresses.}
#'   \item{timeoffset}{Time offset from network.}
#'   \item{connections}{Number of active connections.}
#'   \item{relayfee}{Minimum relay fee.}
#'   \item{...}{Other network parameters.}
#'
#' @examples
#' \dontrun{
#' net_info <- mc_get_network_info(conn)
#' cat("Listening port:", net_info$localaddresses[[1]]$port)
#' }
#'
#' @seealso \code{\link{mc_get_peer_info}} for peer details.
#'
#' @family networking
#' @export
mc_get_network_info <- function(conn) {
  mc_rpc(conn, "getnetworkinfo")
}

#' Get information about connected peers
#'
#' Returns detailed information about each peer currently connected to the node.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with one row per peer.
#'   Common columns include:
#'   \item{addr}{Peer address and port.}
#'   \item{addrlocal}{Local address used for the connection.}
#'   \item{services}{Services offered.}
#'   \item{lastsend}{Last time a message was sent.}
#'   \item{lastrecv}{Last time a message was received.}
#'   \item{bytessent}{Total bytes sent.}
#'   \item{bytesrecv}{Total bytes received.}
#'   \item{conntime}{Connection start time.}
#'   \item{pingtime}{Ping time (seconds).}
#'   \item{version}{Peer's version.}
#'   \item{subver}{Peer's subversion string.}
#'   \item{inbound}{Whether the peer connected inbound.}
#'
#' @examples
#' \dontrun{
#' peers <- mc_get_peer_info(conn)
#' print(head(peers))
#' }
#'
#' @seealso \code{\link{mc_ping}} to measure latency,
#'   \code{\link{mc_get_network_info}} for node network status.
#'
#' @family networking
#' @export
mc_get_peer_info <- function(conn) {
  res <- mc_rpc(conn, "getpeerinfo")
  rpc_res_to_df(res)
}

#' List known peer node addresses (MultiChain 2.3+)
#'
#' Returns a list of nodes that the node has stored in its address manager.
#' This includes peers that were connected to or manually added.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param include_old_ignores Logical. If \code{TRUE}, includes nodes that
#'   were previously ignored. Default is \code{FALSE}.
#'
#' @return A data frame (via \code{rpc_res_to_df}) of stored nodes.
#'
#' @examples
#' \dontrun{
#' nodes <- mc_list_stored_nodes(conn)
#' }
#'
#' @seealso \code{\link{mc_store_node}} to add a node to the stored list.
#'
#' @family networking
#' @export
mc_list_stored_nodes <- function(conn, include_old_ignores = FALSE) {
  res <- mc_rpc(conn, "liststorednodes", list(include_old_ignores))
  rpc_res_to_df(res)
}

#' Ping all connected peers
#'
#' Sends a ping message to all connected peers. The ping time (latency) is
#' recorded and can be viewed in the \code{pingtime} column of
#' \code{\link{mc_get_peer_info}}.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return Invisibly returns \code{NULL} on success.
#'
#' @examples
#' \dontrun{
#' mc_ping(conn)
#' Sys.sleep(1)
#' peers <- mc_get_peer_info(conn)
#' print(peers$pingtime)
#' }
#'
#' @seealso \code{\link{mc_get_peer_info}} to view ping times.
#'
#' @family networking
#' @export
mc_ping <- function(conn) {
  mc_rpc(conn, "ping")
}

#' Add an IP address to known peer nodes (MultiChain 2.3+)
#'
#' Stores a node address in the node's address manager. This can be used to
#' manually add a peer for future connections or to ignore a node.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param node Character string. The IP address and port of the peer node.
#' @param command Character string. Action to perform:
#'   \itemize{
#'     \item \code{"tryconnect"} – store the node and try to connect to it
#'           (default).
#'     \item \code{"ignore"} – ignore the node (do not attempt connections).
#'   }
#'
#' @return Invisibly returns \code{NULL} on success.
#'
#' @examples
#' \dontrun{
#' # Add a node to the address manager
#' mc_store_node(conn, "192.168.1.20:8571", command = "tryconnect")
#' }
#'
#' @seealso \code{\link{mc_list_stored_nodes}} to list stored nodes.
#'
#' @family networking
#' @export
mc_store_node <- function(conn, node, command = c("tryconnect", "ignore")) {
  command <- match.arg(command)
  mc_rpc(conn, "storenode", list(node, command))
}
