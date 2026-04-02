#' Internal function for JSON-RPC requests
#' 
#' Handles the low-level HTTP POST requests to the MultiChain node. It manages 
#' authentication, scientific notation suppression for transaction IDs, 
#' retry logic, and error handling for common RPC failure states.
#' 
#' @param conn A \code{multichain_conn} object containing the node URL and credentials.
#' @param method Character string. The specific MultiChain RPC method to call.
#' @param params List. A collection of parameters expected by the RPC method.
#' 
#' @details 
#' The function temporarily sets \code{options(scipen = 99)} to ensure that 
#' large numbers or long hex strings (like txids) are not converted to 
#' scientific notation when serialized to JSON.
#' 
#' @return The \code{result} field from the JSON-RPC response.
#' 
#' @importFrom httr2 request req_auth_basic req_body_json req_retry req_error req_perform resp_status resp_body_json
#' @keywords internal
mc_rpc <- function(conn, method, params = list()) {
  
  if (!inherits(conn, "multichain_conn")) {
    stop("Argument 'conn' must be a 'multichain_conn' object.", call. = FALSE)
  }
  
  # Prevent scientific notation in JSON serialization
  old_scipen <- getOption("scipen")
  options(scipen = 99)
  on.exit(options(scipen = old_scipen), add = TRUE)
  
  body <- list(method = method, params = params, id = 1)
  
  req <- httr2::request(conn$url) %>%
    httr2::req_auth_basic(conn$user, conn$password) %>%
    httr2::req_body_raw(
      jsonlite::toJSON(body, auto_unbox = TRUE, digits = 22), 
      "application/json"
    ) %>%
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
    httr2::resp_body_json(resp, check_type = FALSE, simplifyVector = FALSE)
  }, error = function(e) {
    stop(sprintf("MultiChain Error: Empty or invalid response from node (Status: %s).", status), call. = FALSE)
  })
  
  if (!is.null(content$error)) {
    err_msg <- if (!is.null(content$error$message)) content$error$message else "Unknown RPC error"
    err_code <- if (!is.null(content$error$code)) content$error$code else "unknown"
    
    stop(sprintf("MultiChain RPC Error [%s]: %s (Code: %s)", 
                 method, err_msg, err_code), call. = FALSE)
  }
  
  return(content$result)
}

#' Convert RPC result list to data frame
#' 
#' Transforms complex, potentially nested list responses from the MultiChain RPC 
#' into a flat or semi-flat data frame.
#' 
#' @param res A list of objects or primitives returned by \code{\link{mc_rpc}}.
#' 
#' @details 
#' If the response contains nested objects, they are preserved as list-columns 
#' using \code{I(list(val))}. If the input is a simple list of vectors, 
#' it is converted into a single-column data frame named \code{value}.
#' 
#' @return A data frame. Returns an empty data frame if the input is \code{NULL} or empty.
#' @keywords internal
rpc_res_to_df <- function(res) {
  if (is.null(res) || length(res) == 0) return(data.frame())
  
  if (!is.list(res[[1]]) && is.null(names(res))) {
    return(data.frame(value = unname(unlist(res)), stringsAsFactors = FALSE))
  }
  
  if (!is.null(names(res))) {
    res <- list(res)
  }
  
  all_names <- unique(unlist(lapply(res, names)))
  
  if (is.null(all_names)) {
    return(data.frame(value = unname(unlist(res)), stringsAsFactors = FALSE))
  }
  
  df_list <- lapply(res, function(x) {
    row_data <- lapply(all_names, function(nm) {
      val <- x[[nm]]
      if (is.null(val)) return(NA)
      if (is.list(val)) return(I(list(val))) 
      return(val)
    })
    names(row_data) <- all_names
    return(as.data.frame(row_data, stringsAsFactors = FALSE))
  })
  
  df <- do.call(rbind, df_list)
  rownames(df) <- NULL
  return(df)
}

#' Decode hex string to character
#' 
#' Converts a hexadecimal encoded string back into its original character 
#' representation. This is primarily used for reading human-readable data 
#' published to MultiChain streams.
#'
#' @param hex_str A character string in hexadecimal format.
#' 
#' @details 
#' The function performs a basic validation to ensure the string is a valid 
#' hexadecimal representation (even length and containing only hex characters) 
#' before attempting to convert.
#' 
#' @return A decoded character string. If the input is not a valid hex string 
#' or an error occurs during decoding, the original \code{hex_str} is returned.
#' @keywords internal
hex_to_char <- function(hex_str) {
  if (is.null(hex_str) || nchar(as.character(hex_str)) == 0) return("")
  hex_str <- as.character(hex_str)
  is_valid_hex <- nchar(hex_str) %% 2 == 0 && grepl("^[0-9a-fA-F]+$", hex_str)
  if (!is_valid_hex) return(hex_str)
  
  starts <- seq(1, nchar(hex_str), 2)
  bytes_str <- substring(hex_str, starts, starts + 1)
  ints <- strtoi(bytes_str, 16L)
  raw_val <- as.raw(ints)
  rawToChar(raw_val)
}

#' Null-default operator
#' 
#' This infix operator provides a convenient way to handle \code{NULL} values 
#' by providing a default value.
#' 
#' @param a An object to check for \code{NULL}.
#' @param b The default value to return if \code{a} is \code{NULL}.
#' 
#' @return \code{a} if it is not \code{NULL}, otherwise \code{b}.
#' 
#' @name null_default
#' @rdname null_default
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b
