#' Get blockchain parameters
#'
#' Returns the parameters that were used to initialize this blockchain.
#' These are fixed at chain creation and cannot be changed later.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return A list containing blockchain configuration parameters, such as:
#'   \item{protocolversion}{Protocol version.}
#'   \item{targetblocktime}{Target time between blocks (seconds).}
#'   \item{maxblocksize}{Maximum block size (bytes).}
#'   \item{...}{Other chain-specific parameters.}
#'
#' @examples
#' \dontrun{
#' params <- mc_get_blockchain_params(conn)
#' print(params$targetblocktime)
#' }
#'
#' @seealso \code{\link{mc_get_runtime_params}} for modifiable parameters.
#'
#' @family node configuration
#' @export
mc_get_blockchain_params <- function(conn) {
  mc_rpc(conn, "getblockchainparams")
}

#' Get node runtime parameters
#'
#' Returns the current runtime parameters of the node. These can be changed
#' while the node is running (see \code{\link{mc_set_runtime_param}}).
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return A list of runtime parameters, e.g.:
#'   \item{mining}{Logical; whether mining is enabled.}
#'   \item{maxconnections}{Maximum number of inbound connections.}
#'   \item{...}{Other runtime settings.}
#'
#' @examples
#' \dontrun{
#' runtime <- mc_get_runtime_params(conn)
#' print(runtime$mining)
#' }
#'
#' @seealso \code{\link{mc_set_runtime_param}} to modify parameters,
#'   \code{\link{mc_get_blockchain_params}} for fixed chain parameters.
#'
#' @family node configuration
#' @export
mc_get_runtime_params <- function(conn) {
  mc_rpc(conn, "getruntimeparams")
}

#' Set node runtime parameter
#'
#' Changes a runtime parameter of the node without requiring a restart.
#' Only a predefined set of parameters can be modified.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param name Character string. The name of the parameter to change.
#'   Must be one of:
#'   \describe{
#'     \item{acceptfiltertimeout}{Timeout for filter acceptance.}
#'     \item{autosubscribe}{Automatically subscribe to streams.}
#'     \item{bantx}{Ban transactions from the mempool.}
#'     \item{handshakelocal}{Local handshake behaviour.}
#'     \item{hideknownopdrops}{Hide known opdrops.}
#'     \item{lockadminminerounds}{Lock admin mining rounds.}
#'     \item{lockblock}{Lock block creation.}
#'     \item{lockinlinemetadata}{Lock inline metadata.}
#'     \item{maxshowndata}{Maximum shown data size.}
#'     \item{maxqueryscanitems}{Maximum items to scan in queries.}
#'     \item{mineemptyrounds}{Number of empty mining rounds.}
#'     \item{miningrequirespeers}{Require peers for mining.}
#'     \item{miningturnover}{Mining turnover.}
#'     \item{sendfiltertimeout}{Timeout for sending filters.}
#'   }
#' @param value The new value for the parameter. Type depends on the parameter:
#'   logical, numeric, or character.
#'
#' @return Invisibly returns \code{NULL} on success; throws an error if the
#'   parameter name is invalid or the value is inappropriate.
#'
#' @examples
#' \dontrun{
#' # Turn off auto‑subscription
#' mc_set_runtime_param(conn, "autosubscribe", FALSE)
#'
#' # Set maximum connections to 50
#' mc_set_runtime_param(conn, "maxconnections", 50)
#' }
#'
#' @seealso \code{\link{mc_get_runtime_params}} to inspect current settings.
#'
#' @family node configuration
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
#' Returns comprehensive information about the node's status, including
#' version, protocol, network connections, balance, and mining status.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return A list with node information, typically including:
#'   \item{version}{Node software version.}
#'   \item{protocolversion}{Protocol version.}
#'   \item{walletversion}{Wallet version.}
#'   \item{balance}{Node's wallet balance.}
#'   \item{blocks}{Current block height.}
#'   \item{timeoffset}{Time offset from network.}
#'   \item{connections}{Number of active connections.}
#'   \item{...}{Other status details.}
#'
#' @examples
#' \dontrun{
#' info <- mc_get_info(conn)
#' cat("Balance:", info$balance, "\n")
#' cat("Blocks:", info$blocks, "\n")
#' }
#'
#' @seealso \code{\link{mc_get_blockchain_info}} for blockchain-level info,
#'   \code{\link{mc_get_runtime_params}} for runtime settings.
#'
#' @family node information
#' @export
mc_get_info <- function(conn) {
  mc_rpc(conn, "getinfo")
}

#' Get node initialization status
#'
#' Returns information about the node's initialization progress,
#' especially useful during startup when the node is still syncing
#' or loading the wallet.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#'
#' @return A list with:
#'   \item{status}{Character string describing the current state (e.g.,
#'     \code{"Loading blockchain"}, \code{"Synchronizing"}, \code{"Ready"}).}
#'   \item{progress}{Numeric value between 0 and 1 indicating initialization
#'     progress (1 = fully initialized).}
#'
#' @examples
#' \dontrun{
#' init <- mc_get_init_status(conn)
#' while (init$progress < 1) {
#'   cat("Init progress:", init$progress, "\n")
#'   Sys.sleep(5)
#'   init <- mc_get_init_status(conn)
#' }
#' }
#'
#' @seealso \code{\link{mc_get_info}} for general node status.
#'
#' @family node information
#' @export
mc_get_init_status <- function(conn) {
  mc_rpc(conn, "getinitstatus")
}

#' Get help for MultiChain commands
#'
#' Returns a list of all available RPC commands, or detailed help for a
#' specific command. The result is printed in a human‑readable format.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param command Optional character string. The name of the command to get
#'   detailed help for. If \code{NULL} (default), returns a list of all commands.
#'
#' @return An object of class \code{"mc_help"} (a character string) that prints
#'   nicely using \code{\link{print.mc_help}}.
#'
#' @examples
#' \dontrun{
#' # List all available commands
#' mc_help(conn)
#'
#' # Get detailed help for the "getinfo" command
#' mc_help(conn, "getinfo")
#' }
#'
#' @seealso \code{\link{mc_rpc}} for direct RPC calls.
#'
#' @family node utilities
#' @export
mc_help <- function(conn, command = NULL) {
  params <- if (is.null(command)) list() else list(command)
  
  res <- mc_rpc(conn, "help", params)
  
  class(res) <- c("mc_help", "character")
  return(res)
}

#' Print MultiChain help
#'
#' S3 method for printing objects returned by \code{\link{mc_help}}.
#' Displays the help text in a clean format.
#'
#' @param x An object of class \code{"mc_help"}.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns \code{x}.
#'
#' @method print mc_help
#' @export
print.mc_help <- function(x, ...) {
  cat(as.character(x), "\n")
  invisible(x)
}
