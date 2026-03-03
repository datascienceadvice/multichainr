#' Get blockchain parameters
#' 
#' Returns an object containing the parameters used to initialize this blockchain.
#'
#' @param conn A multichain_conn object.
#' @return A list containing blockchain configuration parameters (e.g., protocol version, 
#' target block time, maximum block size, etc.).
#' @export
mc_get_blockchain_params <- function(conn) {
  mc_rpc(conn, "getblockchainparams")
}

#' Get node runtime parameters
#' 
#' Returns an object containing the current runtime parameters of the node.
#'
#' @param conn A multichain_conn object.
#' @return A list of runtime parameters (e.g., mining status, connection limits, etc.).
#' @export
mc_get_runtime_params <- function(conn) {
  mc_rpc(conn, "getruntimeparams")
}

#' Set node runtime parameter
#' 
#' Changes a runtime parameter of the node without requiring a restart.
#'
#' @param conn A multichain_conn object.
#' @param name The name of the parameter to change. Supported: acceptfiltertimeout, 
#' autosubscribe, bantx, handshakelocal, hideknownopdrops, lockadminminerounds, 
#' lockblock, lockinlinemetadata, maxshowndata, maxqueryscanitems, 
#' mineemptyrounds, miningrequirespeers, miningturnover, sendfiltertimeout.
#' @param value The new value for the parameter. Can be boolean, numeric, or string.
#' @return NULL on success.
#' @export
mc_set_runtime_param <- function(conn, name, value) {
  supported_params <- c(
    "acceptfiltertimeout", "autosubscribe", "bantx", "handshakelocal",
    "hideknownopdrops", "lockadminminerounds", "lockblock",
    "lockinlinemetadata", "maxshowndata", "maxqueryscanitems",
    "mineemptyrounds", "miningrequirespeers", "miningturnover",
    "sendfiltertimeout"
  )
  
  name <- match.arg(name, supported_params)
  
  mc_rpc(conn, "setruntimeparam", list(name, value))
}

#' Get general node information
#'
#' Returns an object containing information about the status of the node, 
#' network, and blockchain.
#'
#' @param conn A multichain_conn object.
#' @return A list containing node parameters (version, protocol, balance, etc.).
#' @export
mc_get_info <- function(conn) {
  mc_rpc(conn, "getinfo")
}

#' Get node initialization status
#' 
#' Returns information about the node's initialization status, 
#' which is especially useful during startup.
#'
#' @param conn A multichain_conn object.
#' @return A list containing status (string) and progress (numeric, 0 to 1).
#' @export
mc_get_init_status <- function(conn) {
  mc_rpc(conn, "getinitstatus")
}

#' Get help for MultiChain commands
#' 
#' Returns a list of all available commands, or detailed help for a specific command.
#'
#' @param conn A multichain_conn object.
#' @param command Optional string. The name of the command to get help for.
#' @return An object of class \code{mc_help} containing the help text.
#' @export
mc_help <- function(conn, command = NULL) {
  params <- if (is.null(command)) list() else list(command)
  
  res <- mc_rpc(conn, "help", params)
  
  class(res) <- c("mc_help", "character")
  return(res)
}

#' Print MultiChain help
#' 
#' @param x An mc_help object.
#' @param ... Additional arguments.
#' @export
#' @method print mc_help
print.mc_help <- function(x, ...) {
  cat(as.character(x), "\n")

  invisible(x)
}