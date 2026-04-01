#' Create a MultiChain connection object
#'
#' Establishes a connection to a MultiChain node by constructing an RPC endpoint.
#' The function accepts either explicit parameters (host, port, user, password)
#' or a configuration list (typically obtained from \code{\link{mc_get_config}}).
#'
#' @param host Either a character string with the IP address or hostname of the
#'   MultiChain node, or a configuration list (as returned by
#'   \code{\link{mc_get_config}}) containing \code{host}, \code{port},
#'   \code{user}, and \code{password}. When a list is provided, the other
#'   arguments are ignored.
#' @param port Integer. RPC port number. Required unless \code{host} is a list.
#' @param user Character string. RPC username. Required unless \code{host} is a list.
#' @param password Character string. RPC password. Required unless \code{host} is a list.
#'
#' @return An object of class \code{"multichain_conn"} containing the RPC URL,
#'   username, and password (the password is stored but hidden in printing).
#'
#' @examples
#' \dontrun{
#' # Using explicit parameters
#' conn <- mc_connect(host = "127.0.0.1", port = 8570,
#'                    user = "multichainrpc", password = "mysecret")
#'
#' # Using a configuration object from mc_get_config
#' config <- mc_get_config("my_chain")
#' conn <- mc_connect(config)
#' }
#'
#' @seealso \code{\link{mc_get_config}} to obtain a configuration list,
#'   \code{\link{print.multichain_conn}} for printing connections.
#'
#' @family connection management
#' @export
mc_connect <- function(host = "127.0.0.1", port = NULL, user = NULL, password = NULL) {
  
  if (is.list(host) && !is.null(host$user)) {
    port     <- host$port
    user     <- host$user
    password <- host$password
    host     <- host$host %||% "127.0.0.1"
  }
  
  if (is.null(port) || is.null(user) || is.null(password)) {
    stop("Must provide port, user, and password (or a config object).")
  }
  
  url <- paste0("http://", host, ":", port)
  
  structure(
    list(
      url = url,
      user = user,
      password = password
    ),
    class = "multichain_conn"
  )
}

#' Print MultiChain connection
#'
#' S3 method for printing \code{multichain_conn} objects. Hides the password
#' for security.
#'
#' @param x An object of class \code{"multichain_conn"}.
#' @param ... Additional arguments passed to \code{print} (ignored).
#'
#' @return Invisibly returns the object \code{x}.
#'
#' @examples
#' \dontrun{
#' conn <- mc_connect(config)
#' print(conn)   # or simply conn
#' }
#'
#' @seealso \code{\link{mc_connect}} for creating connections.
#'
#' @method print multichain_conn
#' @export
print.multichain_conn <- function(x, ...) {
  cat("<MultiChain Connection>\n")
  cat("URL: ", x$url, "\n")
  cat("User:", x$user, "\n")
  cat("Password: [HIDDEN]\n")
  invisible(x)
}
