#' Set path to MultiChain binaries
#'
#' This function sets the global option `multichain.path` to the directory
#' containing the MultiChain executables (`multichaind` and `multichain-util`).
#' All other functions that need to locate the binaries will use this option.
#'
#' @param path Character string. Path to the folder containing the MultiChain
#'   executables. Must be an existing directory.
#'
#' @return Invisibly returns the normalized path (as set in the option) or
#'   throws an error if the directory does not exist.
#'
#' @examples
#' \dontrun{
#' # Set path to MultiChain installation (example on Unix-like systems)
#' mc_set_path("/usr/local/bin")
#'
#' # Check that the option was set correctly
#' getOption("multichain.path")
#' }
#'
#' @seealso \code{\link{mc_get_bin_path}} for the internal path resolution,
#'   and \code{\link{mc_connect}} for establishing a connection.
#'
#' @family path management
#' @export
mc_set_path <- function(path) {
  if (!dir.exists(path)) stop("Directory does not exist.")
  options(multichain.path = normalizePath(path, mustWork = TRUE))
}

#' Internal function to locate executable files
#'
#' Searches for a MultiChain binary (e.g., `multichaind` or `multichain-util`)
#' first in the path set by `mc_set_path()`, then in the system `PATH`.
#' This function is not intended to be called directly by users.
#'
#' @param bin_name Character string. Name of the binary (e.g., `"multichaind"`,
#'   `"multichain-util"`). The function automatically appends `.exe` on Windows.
#'
#' @return Character string with the full path to the binary, or stops with an
#'   error if the binary is not found.
#'
#' @keywords internal
mc_get_bin_path <- function(bin_name) {
  if (.Platform$OS.type == "windows") {
    bin_name <- paste0(bin_name, ".exe")
  }
  
  opt_path <- getOption("multichain.path")
  
  if (is.null(opt_path)) {
    opt_path <- Sys.getenv("MULTICHAIN_PATH", unset = "")
    if (opt_path == "") opt_path <- NULL # Приводим к единому виду
  }
  
  if (!is.null(opt_path)) {
    full_path <- file.path(opt_path, bin_name)
    if (file.exists(full_path)) return(normalizePath(full_path))
  }
  
  system_path <- Sys.which(bin_name)
  if (system_path != "") return(system_path)
  
  stop(
    sprintf("File '%s' not found. \nSet path via options(multichain.path = '...') or MULTICHAIN_PATH in .Renviron.", bin_name), 
    call. = FALSE
  )
}

#' Get MultiChain configuration
#'
#' Reads the configuration parameters (RPC user, password, port) for a given
#' blockchain from the MultiChain data directory. The function automatically
#' determines the platform‑specific base directory, but a custom base can be
#' supplied for testing.
#'
#' @param chain_name Character string. Name of the MultiChain blockchain.
#' @param base_dir Optional character string. Base directory where MultiChain
#'   stores blockchain data. If `NULL` (default), the platform‑specific default
#'   is used:
#'   * Windows: `%APPDATA%/MultiChain`
#'   * macOS: `~/Library/Application Support/MultiChain`
#'   * Linux/other: `~/.multichain`
#'
#' @return A list with four components:
#'   \item{user}{RPC username (from `multichain.conf`).}
#'   \item{password}{RPC password (from `multichain.conf`).}
#'   \item{port}{RPC port number (integer).}
#'   \item{host}{Always `"127.0.0.1"` (hard‑coded).}
#'
#' @examples
#' \dontrun{
#' # Get configuration for a chain called "my_chain"
#' config <- mc_get_config("my_chain")
#' print(config)
#' }
#'
#' @seealso \code{\link{mc_connect}} to create a connection object using the
#'   returned configuration.
#'
#' @family configuration
#' @export
mc_get_config <- function(chain_name, base_dir = NULL) {
  if (is.null(base_dir)) {
    home <- Sys.getenv("HOME")
    if (.Platform$OS.type == "windows") {
      base_dir <- file.path(Sys.getenv("APPDATA"), "MultiChain")
    } else if (Sys.info()["sysname"] == "Darwin") {
      base_dir <- file.path(home, "Library/Application Support/MultiChain")
    } else {
      base_dir <- file.path(home, ".multichain")
    }
  }
  
  chain_dir <- file.path(base_dir, chain_name)
  if (!dir.exists(chain_dir)) stop("Blockchain directory not found: ", chain_dir)
  
  find_val <- function(file_path, key, separator = "=") {
    if (!file.exists(file_path)) return(NULL)
    lines <- readLines(file_path, warn = FALSE)
    line <- grep(paste0("^", key, separator), lines, value = TRUE)
    if (length(line) == 0) return(NULL)
    trimws(sub(paste0("^", key, separator), "", line[1]))
  }
  
  user <- find_val(file.path(chain_dir, "multichain.conf"), "rpcuser")
  pass <- find_val(file.path(chain_dir, "multichain.conf"), "rpcpassword")
  port <- find_val(file.path(chain_dir, "multichain.conf"), "rpcport")
  
  if (is.null(port)) {
    params_path <- file.path(chain_dir, "params.dat")
    lines <- readLines(params_path, warn = FALSE)
    line <- grep("^default-rpc-port\\s*=", lines, value = TRUE)
    port <- as.numeric(sub(".*=\\s*(\\d+).*", "\\1", line[1]))
  }
  
  return(list(user = user, password = pass, port = as.integer(port), host = "127.0.0.1"))
}

#' Initialize a new MultiChain blockchain
#'
#' Creates a new blockchain using the `multichain-util` command. The new
#' blockchain is set up in the MultiChain data directory (platform‑specific).
#'
#' @param chain_name Character string. Name of the blockchain to create.
#'
#' @return Invisibly returns the output of the `multichain-util create` command
#'   (a character vector). If the creation fails, the function stops with an
#'   error.
#'
#' @examples
#' \dontrun{
#' # Create a blockchain called "my_chain"
#' mc_node_init("my_chain")
#' }
#'
#' @seealso \code{\link{mc_node_start}} to start the created node,
#'   \code{\link{mc_node_stop}} to stop it.
#'
#' @family node operations
#' @export
mc_node_init <- function(chain_name) {
  bin <- mc_get_bin_path("multichain-util")
  message("Creating blockchain: ", chain_name)
  result <- system2(bin, args = c("create", chain_name), stdout = TRUE, stderr = TRUE)
  if (any(grepl("error", result, ignore.case = TRUE))) stop("Creation error")
  message("Blockchain created")
  return(invisible(result))
}

#' Start a MultiChain node
#'
#' Launches a MultiChain node for a given blockchain. The node is started in
#' daemon mode (`-daemon`). If a custom data directory is provided, it is passed
#' via the `-datadir` argument.
#'
#' @param chain_name Character string. Name of the blockchain to start.
#' @param datadir Optional character string. Custom data directory for the
#'   blockchain. If `NULL` (default), the default MultiChain data location is
#'   used.
#'
#' @return Invisibly returns `TRUE` after issuing the start command.
#'
#' @examples
#' \dontrun{
#' # Start the node for "my_chain"
#' mc_node_start("my_chain")
#'
#' # Start with a custom data directory
#' mc_node_start("my_chain", datadir = "/path/to/data")
#' }
#'
#' @seealso \code{\link{mc_node_init}} to create the blockchain,
#'   \code{\link{mc_node_stop}} to stop the node.
#'
#' @family node operations
#' @export
mc_node_start <- function(chain_name, datadir = NULL) {
  bin <- mc_get_bin_path("multichaind")
  args <- c(chain_name, "-daemon")
  if (!is.null(datadir)) args <- c(args, paste0("-datadir=", datadir))
  message("Starting node ", chain_name, "...")
  system2(bin, args = args, wait = FALSE)
  Sys.sleep(2)
  message("Start command issued.")
  return(invisible(TRUE))
}

#' Stop a MultiChain node
#'
#' Stops a running MultiChain node. The function accepts either a connection
#' object (created by `mc_connect()`) or a chain name. When a chain name is
#' provided, it first retrieves the configuration and establishes a connection
#' automatically.
#'
#' @param x Either a character string (the chain name) or an object of class
#'   `"multichain_conn"` (a connection created by `mc_connect()`).
#'
#' @return Invisibly returns the result of the RPC `stop` command.
#'
#' @examples
#' \dontrun{
#' # Stop by chain name
#' mc_node_stop("my_chain")
#'
#' # Stop using a connection object
#' conn <- mc_connect(mc_get_config("my_chain"))
#' mc_node_stop(conn)
#' }
#'
#' @seealso \code{\link{mc_connect}}, \code{\link{mc_rpc}},
#'   \code{\link{mc_node_start}} to start a node.
#'
#' @family node operations
#' @export
mc_node_stop <- function(x) {
  if (is.character(x)) {
    conf <- mc_get_config(x)
    conn <- mc_connect(conf)
    result <- mc_rpc(conn, "stop")
  } else if (inherits(x, "multichain_conn")) {
    result <- mc_rpc(x, "stop")
  } else {
    stop("Invalid input.")
  }
  message("Stop signal sent.")
  return(invisible(result))
}
