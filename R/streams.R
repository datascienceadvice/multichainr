#' Create new stream
#' 
#' @param conn Connection object.
#' @param name Stream name.
#' @param open Boolean. If TRUE, anyone with "send" metadata permissions can publish.
#' @export
mc_create_stream <- function(conn, name, open = TRUE) {
  params <- list("stream", name, open)
  mc_rpc(conn, "create", params)
}

#' Subscribe to a stream
#' 
#' Node must be subscribed to a stream to list its items.
#' @param conn Connection object.
#' @param stream Stream name.
#' @export
mc_subscribe <- function(conn, stream) {
  mc_rpc(conn, "subscribe", list(stream))
}

#' Publish data to a stream
#' 
#' @param conn Connection object.
#' @param stream Stream name.
#' @param key Item key.
#' @param data Data string or list (will be converted to JSON).
#' @export
mc_publish <- function(conn, stream, key, data) {
  if (is.list(data)) {
    data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  }
  
  data_hex <- paste(charToRaw(as.character(data)), collapse = "")
  params <- list(stream, key, data_hex)
  mc_rpc(conn, "publish", params)
}

#' List items in a stream
#' 
#' @param conn Connection object.
#' @param stream Stream name.
#' @param count Number of items to retrieve.
#' @export
mc_list_stream_items <- function(conn, stream, count = 10) {
  items <- mc_rpc(conn, "liststreamitems", list(stream, FALSE, count))
  
  if (length(items) == 0) {
    return(data.frame())
  }
  
  processed_items <- lapply(items, function(item) {
    txt <- hex_to_char(item$data)

    if (startsWith(txt, "{") || startsWith(txt, "[")) {
      out <- try(jsonlite::fromJSON(txt, simplifyVector = FALSE), silent = TRUE)
      if (!inherits(out, "try-error")) item$data_decoded <- txt # добавим колонку
    }
    item$data <- txt
    
    item$time <- as.POSIXct(item$blocktime %||% item$time %||% as.numeric(Sys.time()), 
                            origin = "1970-01-01")
    
    item$publishers <- paste(unlist(item$publishers), collapse = ",")
    
    return(item)
  })
  
  rpc_res_to_df(processed_items)
}