#' Create a MultiChain connection object
#'
#' @param host IP address or a config list from mc_get_config.
#' @param port RPC port number.
#' @param user RPC username.
#' @param password RPC password.
#'
#' @return A \code{multichain_conn} object.
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
#' @param x A multichain_conn object.
#' @param ... Additional arguments passed to print.
#' @export
print.multichain_conn <- function(x, ...) {
  cat("<MultiChain Connection>\n")
  cat("URL: ", x$url, "\n")
  cat("User:", x$user, "\n")
  cat("Password: [HIDDEN]\n")
}