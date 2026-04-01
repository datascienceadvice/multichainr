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
  
  old_scipen <- getOption("scipen")
  options(scipen = 99)
  on.exit(options(scipen = old_scipen), add = TRUE)
  
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
    err_msg <- if (!is.null(content$error$message)) content$error$message else "Unknown RPC error"
    err_code <- if (!is.null(content$error$code)) content$error$code else "unknown"
    
    stop(sprintf("MultiChain RPC Error [%s]: %s (Code: %s)", 
                 method, err_msg, err_code), call. = FALSE)
  }
  
  return(content$result)
}

#' Convert RPC result list to data frame
#' 
#' @param res A list of objects or primitives returned by [mc_rpc()].
#' @return A data frame. Returns an empty data frame if the input is empty.
#' @keywords internal
rpc_res_to_df <- function(res) {
  if (is.null(res) || length(res) == 0) return(data.frame())
  
  all_names <- unique(unlist(lapply(res, names)))
  
  # case 1: list of vectors
  if (is.null(all_names)) {
    df <- data.frame(value = unname(unlist(res)), stringsAsFactors = FALSE)
    return(df)
  }
  
  # case 2: list of objects
  df_list <- lapply(res, function(x) {
    row_list <- lapply(all_names, function(name) {
      val <- x[[name]]
      
      if (is.null(val)) return(NA)
      
      if (is.list(val)) return(I(list(val)))
      
      return(val)
    })
    
    names(row_list) <- all_names
    as.data.frame(row_list, stringsAsFactors = FALSE)
  })
  
  df <- do.call(rbind, df_list)
  rownames(df) <- NULL
  return(df)
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