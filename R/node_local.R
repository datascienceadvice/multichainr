#' Set path to MultiChain binaries
#' 
#' @param path Path to the folder containing multichaind and multichain-util.
#' @export
mc_set_path <- function(path) {
  if (!dir.exists(path)) stop("Directory does not exist.")
  options(multichainr.path = normalizePath(path, mustWork = TRUE))
}

#' Internal function to locate executable files
#' @param bin_name Name of the binary (multichaind or multichain-util).
#' @keywords internal
mc_get_bin_path <- function(bin_name) {
  if (.Platform$OS.type == "windows") {
    bin_name <- paste0(bin_name, ".exe")
  }
  opt_path <- getOption("multichainr.path")
  if (!is.null(opt_path)) {
    full_path <- file.path(opt_path, bin_name)
    if (file.exists(full_path)) return(full_path)
  }
  system_path <- Sys.which(bin_name)
  if (system_path != "") return(system_path)
  stop(sprintf("File '%s' not found. Use mc_set_path().", bin_name), call. = FALSE)
}

#' Get MultiChain config
#' 
#' @param chain_name Name of the chain.
#' @param base_dir Optional base directory path (for testing).
#' @export
mc_get_config <- function(chain_name, base_dir = NULL) {
  if (is.null(base_dir)) {
    home <- Sys.getenv("HOME")
    if (.Platform$OS.type == "windows") {
      base_dir <- file.path(Sys.getenv("APPDATA"), "MultiChain")
    } else if (Sys.info()['sysname'] == "Darwin") {
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

#' Initialize new MultiChain blockchain
#' 
#' @param chain_name Name of the blockchain to create.
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
#' @param chain_name Name of the blockchain to start.
#' @param datadir Optional custom data directory.
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
#' @param x Either a connection object or a chain name.
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