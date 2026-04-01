#' Internal function for JSON-RPC requests
#' 
#' @param conn A multichain_conn object.
#' @param method Character string of the RPC method.
#' @param params List of parameters.
#' 
#' @return The result field from the JSON-RPC response.
#' @keywords internal
mc_rpc <- function(conn, method, params = list()) {
  
  if (!inherits(conn, "multichain_conn")) {
    stop("Argument 'conn' must be a 'multichain_conn' object.", call. = FALSE)
  }
  
  old_scipen <- options(scipen = 99)
  on.exit(options(old_scipen), add = TRUE)
  
  body <- list(method = method, params = params, id = 1)
  
  req <- httr2::request(conn$url) %>%
    httr2::req_auth_basic(conn$user, conn$password) %>%
    httr2::req_body_json(body) %>%
    httr2::req_retry(max_tries = 3) %>%
    httr2::req_error(is_error = function(resp) FALSE)
  
  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    stop(paste("Failed to connect to MultiChain node:", e$message), call. = FALSE)
  })
  
  status <- httr2::resp_status(resp)
  if (status == 401) {
    stop("MultiChain Error: 401 Unauthorized (check RPC username and password).", call. = FALSE)
  }
  
  content <- tryCatch({
    httr2::resp_body_json(resp, check_type = FALSE)
  }, error = function(e) {
    stop(sprintf("MultiChain Error: Empty or invalid response from node (Status: %s).", status), call. = FALSE)
  })
  
  if (!is.null(content$error)) {
    stop(sprintf("MultiChain RPC Error [%s]: %s (Code: %s)", 
                 method, content$error$message, content$error$code %||% "unknown"), call. = FALSE)
  }
  
  return(content$result)
}

#' Convert RPC result list to data frame
#' 
#' A helper function that takes a list of lists (typically returned by MultiChain RPC 
#' calls like `listassets` or `listpermissions`) and converts it into a flat 
#' data frame.
#'
#' @param res A list of objects returned by [mc_rpc()].
#' @return A data frame where each element of the list is a row. 
#' Returns an empty data frame if the input is empty.
#' @keywords internal
rpc_res_to_df <- function(res) {
  if (is.null(res) || length(res) == 0) return(data.frame())
  
  all_names <- unique(unlist(lapply(res, names)))
  
  df_list <- lapply(res, function(x) {
    missing <- setdiff(all_names, names(x))
    x[missing] <- NA
    x <- x[all_names]
    x[sapply(x, is.null)] <- NA
    as.data.frame(x, stringsAsFactors = FALSE)
  })
  
  do.call(rbind, df_list)
}

#' Decode hex string to character
#' 
#' Converts a hexadecimal encoded string back into its original character 
#' representation. This is commonly used to read data published to MultiChain streams.
#'
#' @param hex_str A character string in hexadecimal format.
#' @return A decoded character string. If the input is not a valid hex string 
#' or an error occurs during decoding, the original `hex_str` is returned.
#' @keywords internal
hex_to_char <- function(hex_str) {
  if (is.null(hex_str) || nchar(as.character(hex_str)) == 0) return("")
  
  hex_str <- as.character(hex_str)

  is_valid_hex <- nchar(hex_str) %% 2 == 0 && grepl("^[0-9a-fA-F]+$", hex_str)
  
  if (!is_valid_hex) return(hex_str)
  
  res <- tryCatch({
    starts <- seq(1, nchar(hex_str), 2)
    bytes_str <- substring(hex_str, starts, starts + 1)
    
    ints <- strtoi(bytes_str, 16L)
    
    raw_val <- as.raw(ints)
    
    rawToChar(raw_val)
  }, error = function(e) {
    return(hex_str)
  })
  
  return(unname(res))
}

#' Null-default operator
#' 
#' @param a An object
#' @param b An object
#' @return a if not null, else b
#' 
#' @name null_default
#' @rdname null_default
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b