#' Create a new stream
#' 
#' @param conn Connection object.
#' @param name Stream name.
#' @param open Boolean (TRUE for open stream) or a list of restrictions (e.g., list(restrict = "write")).
#' @param custom_fields Optional list of custom fields.
#' @return The txid of the stream creation.
#' @export
mc_create_stream <- function(conn, name, open = TRUE, custom_fields = NULL) {
  # В MultiChain параметр может быть либо boolean (open), либо объектом с параметрами
  params_arg <- if (is.list(open)) open else open
  
  args <- list("stream", name, params_arg)
  if (!is.null(custom_fields)) args <- c(args, list(custom_fields))
  
  mc_rpc(conn, "create", args)
}

#' Create a new stream from a specific address
#' 
#' @param conn Connection object.
#' @param from_address Address used to create the stream.
#' @param name Stream name.
#' @param open Boolean or list of parameters.
#' @param custom_fields Optional list of custom fields.
#' @export
mc_create_stream_from <- function(conn, from_address, name, open = TRUE, custom_fields = NULL) {
  args <- list(from_address, "stream", name, open)
  if (!is.null(custom_fields)) args <- c(args, list(custom_fields))
  
  mc_rpc(conn, "createfrom", args)
}

#' Get information about a specific stream
#' 
#' @param conn Connection object.
#' @param stream Stream name, ref or creation txid.
#' @param verbose If TRUE, includes creator information.
#' @export
mc_get_stream_info <- function(conn, stream, verbose = FALSE) {
  mc_rpc(conn, "getstreaminfo", list(stream, verbose))
}

#' List streams on the blockchain
#' 
#' @param conn Connection object.
#' @param streams Stream filter (name, ref, txid, vector or "*").
#' @param verbose If TRUE, returns detailed information.
#' @param count Number of streams to return.
#' @param start Offset.
#' @export
mc_list_streams <- function(conn, streams = "*", verbose = FALSE, count = NULL, start = NULL) {
  params <- list(streams, verbose)
  
  if (!is.null(count)) {
    params <- c(params, list(as.integer(count)))
    if (!is.null(start)) {
      params <- c(params, list(as.integer(start)))
    }
  } else if (!is.null(start)) {
    # Если указан start, но не count, ставим максимально возможное число для count
    params <- c(params, list(2147483647, as.integer(start)))
  }
  
  res <- mc_rpc(conn, "liststreams", params)
  rpc_res_to_df(res)
}

#' Publish an item to a stream
#' 
#' @param conn Connection object.
#' @param stream Stream name, ref or txid.
#' @param keys A single key (string) or a vector of keys.
#' @param data Data to publish. Can be a hex string, or a list like list(text = "...") or list(json = ...).
#' @param options Optional. Use "offchain" to publish as an off-chain item.
#' @export
mc_publish <- function(conn, stream, keys, data, options = NULL) {
  # Если передано несколько ключей, превращаем в список для JSON-RPC
  keys_param <- if (length(keys) > 1) as.list(keys) else keys
  
  params <- list(stream, keys_param, data)
  if (!is.null(options)) params <- c(params, list(options))
  
  mc_rpc(conn, "publish", params)
}

#' Publish an item to a stream from a specific address
#' 
#' @param conn Connection object.
#' @param from_address Address used to publish the item.
#' @param stream Stream name, ref or txid.
#' @param keys A single key or vector of keys.
#' @param data Data string or list.
#' @param options Optional "offchain" string.
#' @export
mc_publish_from <- function(conn, from_address, stream, keys, data, options = NULL) {
  keys_param <- if (length(keys) > 1) as.list(keys) else keys
  
  params <- list(from_address, stream, keys_param, data)
  if (!is.null(options)) params <- c(params, list(options))
  
  mc_rpc(conn, "publishfrom", params)
}

#' Publish multiple items to a stream in one transaction
#' 
#' @param conn Connection object.
#' @param stream Default stream name, ref or txid.
#' @param items A list of lists, each containing "key" (or "keys") and "data".
#' @param options Optional default "offchain" string.
#' @export
mc_publish_multi <- function(conn, stream, items, options = NULL) {
  params <- list(stream, items)
  if (!is.null(options)) params <- c(params, list(options))
  
  mc_rpc(conn, "publishmulti", params)
}

#' Publish multiple items from a specific address
#' 
#' @param conn Connection object.
#' @param from_address Address used to publish the items.
#' @param stream Default stream.
#' @param items List of lists.
#' @param options Optional default "offchain" string.
#' @export
mc_publish_multi_from <- function(conn, from_address, stream, items, options = NULL) {
  params <- list(from_address, stream, items)
  if (!is.null(options)) params <- c(params, list(options))
  
  mc_rpc(conn, "publishmultifrom", params)
}

#' Get a specific item from a stream
#' @export
mc_get_stream_item <- function(conn, stream, txid, verbose = FALSE) {
  mc_rpc(conn, "getstreamitem", list(stream, txid, verbose))
}

#' Get summary of JSON objects in a stream for a specific key
#' @param mode Default "jsonobjectmerge". Can include "recursive", "noupdate", etc.
#' @export
mc_get_stream_key_summary <- function(conn, stream, key, mode = "jsonobjectmerge") {
  mc_rpc(conn, "getstreamkeysummary", list(stream, key, mode))
}

#' Get summary of JSON objects published by a specific address
#' @export
mc_get_stream_publisher_summary <- function(conn, stream, address, mode = "jsonobjectmerge") {
  mc_rpc(conn, "getstreampublishersummary", list(stream, address, mode))
}

#' List items in a stream (Enhanced version)
#' @export
mc_list_stream_items <- function(conn, stream, verbose = FALSE, count = 10, start = NULL, local_ordering = FALSE) {
  # Обработка пагинации по умолчанию (последние 10 элементов)
  actual_start <- if (is.null(start)) -as.integer(count) else as.integer(start)
  params <- list(stream, verbose, as.integer(count), actual_start, local_ordering)
  
  res <- mc_rpc(conn, "liststreamitems", params)
  rpc_res_to_df(res)
}

#' List items with a specific key
#' @export
mc_list_stream_key_items <- function(conn, stream, key, verbose = FALSE, count = 10, start = NULL, local_ordering = FALSE) {
  actual_start <- if (is.null(start)) -as.integer(count) else as.integer(start)
  params <- list(stream, key, verbose, as.integer(count), actual_start, local_ordering)
  
  res <- mc_rpc(conn, "liststreamkeyitems", params)
  rpc_res_to_df(res)
}

#' List unique keys in a stream
#' @export
mc_list_stream_keys <- function(conn, stream, keys = "*", verbose = FALSE, count = NULL, start = NULL, local_ordering = FALSE) {
  params <- list(stream, keys, verbose)
  if (!is.null(count)) params <- c(params, list(as.integer(count)))
  if (!is.null(start)) {
    if (is.null(count)) params <- c(params, list(2147483647))
    params <- c(params, list(as.integer(start)))
  }
  if (local_ordering) params <- c(params, list(TRUE))
  
  res <- mc_rpc(conn, "liststreamkeys", params)
  rpc_res_to_df(res)
}

#' List items published by a specific address
#' @export
mc_list_stream_publisher_items <- function(conn, stream, address, verbose = FALSE, count = 10, start = NULL, local_ordering = FALSE) {
  actual_start <- if (is.null(start)) -as.integer(count) else as.integer(start)
  params <- list(stream, address, verbose, as.integer(count), actual_start, local_ordering)
  
  res <- mc_rpc(conn, "liststreampublisheritems", params)
  rpc_res_to_df(res)
}

#' List publishers who have written to a stream
#' @export
mc_list_stream_publishers <- function(conn, stream, addresses = "*", verbose = FALSE, count = NULL, start = NULL, local_ordering = FALSE) {
  params <- list(stream, addresses, verbose)
  if (!is.null(count)) params <- c(params, list(as.integer(count)))
  if (!is.null(start)) {
    if (is.null(count)) params <- c(params, list(2147483647))
    params <- c(params, list(as.integer(start)))
  }
  res <- mc_rpc(conn, "liststreampublishers", params)
  rpc_res_to_df(res)
}

#' Query items by matching keys and publishers
#' @param query List like list(keys = c("k1"), publishers = c("addr1"))
#' @export
mc_list_stream_query_items <- function(conn, stream, query, verbose = FALSE) {
  res <- mc_rpc(conn, "liststreamqueryitems", list(stream, query, verbose))
  rpc_res_to_df(res)
}

#' List items in stream within a given transaction ID
#' @export
mc_list_stream_tx_items <- function(conn, stream, txid, verbose = FALSE) {
  res <- mc_rpc(conn, "liststreamtxitems", list(stream, txid, verbose))
  rpc_res_to_df(res)
}

#' List items in a specific block or blocks
#' @param blocks Vector of heights, hashes or timestamps.
#' @export
mc_list_stream_block_items <- function(conn, stream, blocks, verbose = FALSE, count = NULL, start = NULL) {
  params <- list(stream, blocks, verbose)
  if (!is.null(count)) params <- c(params, list(as.integer(count)))
  if (!is.null(start)) params <- c(params, list(as.integer(start)))
  
  res <- mc_rpc(conn, "liststreamblockitems", params)
  rpc_res_to_df(res)
}