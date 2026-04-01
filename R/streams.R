#' Create a new stream
#'
#' Creates a new stream on the blockchain. Streams are ordered collections of
#' key‑value items that can be used for data storage, messaging, or other
#' applications. The stream can be open (anyone can write) or restricted.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param name Character string. Name of the stream (must be unique).
#' @param open Either a logical (TRUE for open stream, FALSE for restricted) or
#'   a list of parameters, e.g., \code{list(restrict = "write")}. For open
#'   streams, any address with \code{send} permission can publish. For restricted,
#'   only addresses with \code{write} permission on the stream can publish.
#' @param custom_fields Optional list of custom fields (e.g., \code{list(field1 = "value")}).
#'
#' @return A character string containing the transaction ID (txid) of the stream creation.
#'
#' @examples
#' \dontrun{
#' # Create an open stream
#' txid <- mc_create_stream(conn, "mystream", open = TRUE)
#'
#' # Create a restricted stream with custom fields
#' txid <- mc_create_stream(conn, "private", open = list(restrict = "write"),
#'                          custom_fields = list(owner = "admin"))
#' }
#'
#' @seealso \code{\link{mc_create_stream_from}} to specify the creator address,
#'   \code{\link{mc_get_stream_info}} to query stream details.
#'
#' @family streams
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
#' Creates a stream from a specified address (the address pays for the transaction).
#' This is useful when the node has multiple addresses and you want to control
#' which address appears as the creator.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. The address that will create the stream.
#' @param name Character string. Stream name.
#' @param open Either logical or a list of parameters (see \code{\link{mc_create_stream}}).
#' @param custom_fields Optional list of custom fields.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' txid <- mc_create_stream_from(conn, "1A...", "mystream", open = TRUE)
#' }
#'
#' @seealso \code{\link{mc_create_stream}}
#'
#' @family streams
#' @export
mc_create_stream_from <- function(conn, from_address, name, open = TRUE, custom_fields = NULL) {
  args <- list(from_address, "stream", name, open)
  if (!is.null(custom_fields)) args <- c(args, list(custom_fields))
  
  mc_rpc(conn, "createfrom", args)
}

#' Get information about a specific stream
#'
#' Returns metadata about a stream, including its creation transaction,
#' open/restricted status, and optionally the creator address.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param stream Character string. Stream name, reference, or creation transaction ID.
#' @param verbose Logical. If \code{TRUE}, includes the creator address.
#'
#' @return A list with stream details (name, ref, open, txid, etc.).
#'
#' @examples
#' \dontrun{
#' info <- mc_get_stream_info(conn, "mystream")
#' print(info$open)
#' }
#'
#' @seealso \code{\link{mc_list_streams}} to list all streams.
#'
#' @family streams
#' @export
mc_get_stream_info <- function(conn, stream, verbose = FALSE) {
  mc_rpc(conn, "getstreaminfo", list(stream, verbose))
}

#' List streams on the blockchain
#'
#' Returns a list of streams (or filtered subset) with optional details.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param streams Character vector of stream names/IDs, or \code{"*"} (default)
#'   for all streams.
#' @param verbose Logical. If \code{TRUE}, returns detailed information.
#' @param count Optional integer. Number of streams to return.
#' @param start Optional integer. Offset for pagination.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with stream information.
#'
#' @examples
#' \dontrun{
#' all_streams <- mc_list_streams(conn)
#' first_10 <- mc_list_streams(conn, count = 10)
#' }
#'
#' @seealso \code{\link{mc_get_stream_info}}
#'
#' @family streams
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
#' Writes a key‑value item to a stream. The item is stored on the blockchain
#' (or optionally off‑chain) and can be retrieved by its key.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param stream Character string. Stream name, reference, or creation txid.
#' @param keys A single key (string) or a vector of keys (for multi‑key items).
#' @param data Data to publish. Can be a hex string, or a list with \code{text}
#'   (will be hex‑encoded) or \code{json} (will be converted to JSON then hex).
#' @param options Optional character string. Use \code{"offchain"} to publish as
#'   an off‑chain item (requires off‑chain capability on the blockchain).
#'
#' @return A character string containing the transaction ID of the published item.
#'
#' @examples
#' \dontrun{
#' # Publish with a text key and simple text data
#' mc_publish(conn, "mystream", "greeting", list(text = "Hello world!"))
#'
#' # Publish with JSON data
#' mc_publish(conn, "mystream", "data", list(json = list(a = 1, b = 2)))
#'
#' # Publish off‑chain
#' mc_publish(conn, "mystream", "large", list(text = "big data"), options = "offchain")
#' }
#'
#' @seealso \code{\link{mc_publish_from}} to specify publisher address,
#'   \code{\link{mc_publish_multi}} for multiple items.
#'
#' @family stream items
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
#' Similar to \code{\link{mc_publish}}, but specifies the publishing address.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. Address that will publish the item.
#' @param stream Character string. Stream name, reference, or txid.
#' @param keys A single key or vector of keys.
#' @param data Data to publish (string or list).
#' @param options Optional \code{"offchain"} string.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' mc_publish_from(conn, "1A...", "mystream", "key", list(text = "data"))
#' }
#'
#' @seealso \code{\link{mc_publish}}
#'
#' @family stream items
#' @export
mc_publish_from <- function(conn, from_address, stream, keys, data, options = NULL) {
  keys_param <- if (length(keys) > 1) as.list(keys) else keys
  
  params <- list(from_address, stream, keys_param, data)
  if (!is.null(options)) params <- c(params, list(options))
  
  mc_rpc(conn, "publishfrom", params)
}

#' Publish multiple items to a stream in one transaction
#'
#' Publishes several key‑value items in a single transaction. This is more
#' efficient than publishing each item separately.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param stream Character string. Default stream for all items (if not overridden
#'   per item).
#' @param items A list of item objects, each containing:
#'   \itemize{
#'     \item \code{key} or \code{keys} – the key(s) for the item.
#'     \item \code{data} – the data to publish.
#'     \item (optional) \code{stream} – override the default stream.
#'   }
#' @param options Optional default \code{"offchain"} string.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' items <- list(
#'   list(key = "k1", data = list(text = "item1")),
#'   list(key = "k2", data = list(json = list(value = 2)))
#' )
#' txid <- mc_publish_multi(conn, "mystream", items)
#' }
#'
#' @seealso \code{\link{mc_publish_multi_from}} for specifying the sender.
#'
#' @family stream items
#' @export
mc_publish_multi <- function(conn, stream, items, options = NULL) {
  params <- list(stream, items)
  if (!is.null(options)) params <- c(params, list(options))
  
  mc_rpc(conn, "publishmulti", params)
}

#' Publish multiple items from a specific address
#'
#' Publishes multiple items in one transaction from a specified address.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. Address that will publish the items.
#' @param stream Character string. Default stream.
#' @param items List of item objects.
#' @param options Optional default \code{"offchain"} string.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' items <- list(list(key = "k1", data = "value1"))
#' txid <- mc_publish_multi_from(conn, "1A...", "mystream", items)
#' }
#'
#' @seealso \code{\link{mc_publish_multi}}
#'
#' @family stream items
#' @export
mc_publish_multi_from <- function(conn, from_address, stream, items, options = NULL) {
  params <- list(from_address, stream, items)
  if (!is.null(options)) params <- c(params, list(options))
  
  mc_rpc(conn, "publishmultifrom", params)
}

#' Get a specific item from a stream
#'
#' Retrieves a single stream item by its transaction ID.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param stream Character string. Stream name, reference, or txid.
#' @param txid Character string. Transaction ID of the item.
#' @param verbose Logical. If \code{TRUE}, includes additional details.
#'
#' @return A list with item details (key, data, publisher, etc.).
#'
#' @examples
#' \dontrun{
#' item <- mc_get_stream_item(conn, "mystream", "txid...")
#' print(item$data)
#' }
#'
#' @seealso \code{\link{mc_list_stream_items}} to list items.
#'
#' @family stream items
#' @export
mc_get_stream_item <- function(conn, stream, txid, verbose = FALSE) {
  mc_rpc(conn, "getstreamitem", list(stream, txid, verbose))
}

#' Get summary of JSON objects in a stream for a specific key
#'
#' Aggregates JSON objects published under a specific key, merging them
#' according to the specified mode.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param stream Character string. Stream name, reference, or txid.
#' @param key Character string. The key to summarize.
#' @param mode Character string. Merge mode (default \code{"jsonobjectmerge"}).
#'   Other options include \code{"recursive"}, \code{"noupdate"}, etc.
#'
#' @return A JSON object (list) representing the merged summary.
#'
#' @examples
#' \dontrun{
#' summary <- mc_get_stream_key_summary(conn, "mystream", "key", mode = "jsonobjectmerge")
#' }
#'
#' @seealso \code{\link{mc_get_stream_publisher_summary}}
#'
#' @family stream summaries
#' @export
mc_get_stream_key_summary <- function(conn, stream, key, mode = "jsonobjectmerge") {
  mc_rpc(conn, "getstreamkeysummary", list(stream, key, mode))
}

#' Get summary of JSON objects published by a specific address
#'
#' Aggregates JSON objects published by a given address in a stream.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param stream Character string. Stream name, reference, or txid.
#' @param address Character string. Publisher address.
#' @param mode Character string. Merge mode (default \code{"jsonobjectmerge"}).
#'
#' @return A JSON object (list) representing the merged summary.
#'
#' @examples
#' \dontrun{
#' summary <- mc_get_stream_publisher_summary(conn, "mystream", "1A...")
#' }
#'
#' @family stream summaries
#' @export
mc_get_stream_publisher_summary <- function(conn, stream, address, mode = "jsonobjectmerge") { # nolint
  mc_rpc(conn, "getstreampublishersummary", list(stream, address, mode))
}

#' List items in a stream (Enhanced version)
#'
#' Returns a list of items published in a stream, with pagination and ordering.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param stream Character string. Stream name, reference, or txid.
#' @param verbose Logical. If \code{TRUE}, returns detailed item information.
#' @param count Integer. Number of items to return (default 10).
#' @param start Optional integer. Offset (positive for forward, negative for backward).
#'   If omitted, the most recent items are returned.
#' @param local_ordering Logical. If \code{TRUE}, uses local node's transaction
#'   ordering (default \code{FALSE}).
#'
#' @return A data frame (via \code{rpc_res_to_df}) with item details.
#'
#' @examples
#' \dontrun{
#' # Get the 10 most recent items
#' items <- mc_list_stream_items(conn, "mystream")
#'
#' # Get next 5 items after the 10th
#' items <- mc_list_stream_items(conn, "mystream", count = 5, start = 10)
#' }
#'
#' @seealso \code{\link{mc_list_stream_key_items}}, \code{\link{mc_list_stream_publisher_items}}
#'
#' @family stream items
#' @export
mc_list_stream_items <- function(conn, stream, verbose = FALSE, count = 10, start = NULL, local_ordering = FALSE) {
  # Обработка пагинации по умолчанию (последние 10 элементов)
  actual_start <- if (is.null(start)) -as.integer(count) else as.integer(start)
  params <- list(stream, verbose, as.integer(count), actual_start, local_ordering)
  
  res <- mc_rpc(conn, "liststreamitems", params)
  rpc_res_to_df(res)
}

#' List items with a specific key
#'
#' Returns all items in a stream that have a given key.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param stream Character string. Stream name, reference, or txid.
#' @param key Character string. The key to filter by.
#' @param verbose Logical. If \code{TRUE}, returns detailed information.
#' @param count Integer. Number of items to return (default 10).
#' @param start Optional integer. Offset for pagination.
#' @param local_ordering Logical. Use local ordering.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with items.
#'
#' @examples
#' \dontrun{
#' items <- mc_list_stream_key_items(conn, "mystream", "mykey")
#' }
#'
#' @family stream items
#' @export
mc_list_stream_key_items <- function(conn, stream, key, verbose = FALSE, count = 10, start = NULL, local_ordering = FALSE) {
  actual_start <- if (is.null(start)) -as.integer(count) else as.integer(start)
  params <- list(stream, key, verbose, as.integer(count), actual_start, local_ordering)
  
  res <- mc_rpc(conn, "liststreamkeyitems", params)
  rpc_res_to_df(res)
}

#' List unique keys in a stream
#'
#' Returns the set of distinct keys used in a stream, with optional pagination.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param stream Character string. Stream name, reference, or txid.
#' @param keys Character vector of keys to filter (default \code{"*"} for all).
#' @param verbose Logical. If \code{TRUE}, returns additional metadata per key.
#' @param count Optional integer. Number of keys to return.
#' @param start Optional integer. Offset.
#' @param local_ordering Logical. Use local ordering.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with key information.
#'
#' @examples
#' \dontrun{
#' keys <- mc_list_stream_keys(conn, "mystream")
#' }
#'
#' @family stream keys
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
#'
#' Returns all items in a stream that were published by a given address.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param stream Character string. Stream name, reference, or txid.
#' @param address Character string. Publisher address.
#' @param verbose Logical. If \code{TRUE}, returns detailed information.
#' @param count Integer. Number of items to return (default 10).
#' @param start Optional integer. Offset.
#' @param local_ordering Logical. Use local ordering.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with items.
#'
#' @examples
#' \dontrun{
#' items <- mc_list_stream_publisher_items(conn, "mystream", "1A...")
#' }
#'
#' @family stream items
#' @export
mc_list_stream_publisher_items <- function(conn, stream, address, verbose = FALSE, count = 10, start = NULL, local_ordering = FALSE) {
  actual_start <- if (is.null(start)) -as.integer(count) else as.integer(start)
  params <- list(stream, address, verbose, as.integer(count), actual_start, local_ordering)
  
  res <- mc_rpc(conn, "liststreampublisheritems", params)
  rpc_res_to_df(res)
}

#' List publishers who have written to a stream
#'
#' Returns the set of addresses that have published to a stream.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param stream Character string. Stream name, reference, or txid.
#' @param addresses Character vector of addresses to filter (default \code{"*"}).
#' @param verbose Logical. If \code{TRUE}, returns additional metadata.
#' @param count Optional integer. Number of publishers to return.
#' @param start Optional integer. Offset.
#' @param local_ordering Logical. Use local ordering.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with publisher information.
#'
#' @examples
#' \dontrun{
#' publishers <- mc_list_stream_publishers(conn, "mystream")
#' }
#'
#' @family stream publishers
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
#'
#' Returns items that match a combination of key and publisher filters.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param stream Character string. Stream name, reference, or txid.
#' @param query A list with optional fields:
#'   \itemize{
#'     \item \code{keys} – vector of keys.
#'     \item \code{publishers} – vector of publisher addresses.
#'   }
#' @param verbose Logical. If \code{TRUE}, returns detailed information.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with matching items.
#'
#' @examples
#' \dontrun{
#' query <- list(keys = c("key1", "key2"), publishers = c("1A..."))
#' items <- mc_list_stream_query_items(conn, "mystream", query)
#' }
#'
#' @family stream queries
#' @export
mc_list_stream_query_items <- function(conn, stream, query, verbose = FALSE) {
  res <- mc_rpc(conn, "liststreamqueryitems", list(stream, query, verbose))
  rpc_res_to_df(res)
}

#' List items in stream within a given transaction ID
#'
#' Returns all stream items that are part of a specific transaction (e.g., when
#' multiple items were published in a single transaction).
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param stream Character string. Stream name, reference, or txid.
#' @param txid Character string. Transaction ID.
#' @param verbose Logical. If \code{TRUE}, returns detailed information.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with items.
#'
#' @examples
#' \dontrun{
#' items <- mc_list_stream_tx_items(conn, "mystream", "txid...")
#' }
#'
#' @family stream items
#' @export
mc_list_stream_tx_items <- function(conn, stream, txid, verbose = FALSE) {
  res <- mc_rpc(conn, "liststreamtxitems", list(stream, txid, verbose))
  rpc_res_to_df(res)
}

#' List items in a specific block or blocks
#'
#' Returns stream items that were published in one or more blocks.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param stream Character string. Stream name, reference, or txid.
#' @param blocks A vector of block heights, block hashes, or timestamps.
#' @param verbose Logical. If \code{TRUE}, returns detailed information.
#' @param count Optional integer. Number of items to return.
#' @param start Optional integer. Offset.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with items.
#'
#' @examples
#' \dontrun{
#' # Items in block height 100
#' items <- mc_list_stream_block_items(conn, "mystream", 100)
#'
#' # Items in multiple blocks
#' items <- mc_list_stream_block_items(conn, "mystream", c(100, 101, 102))
#' }
#'
#' @family stream items
#' @export
mc_list_stream_block_items <- function(conn, stream, blocks, verbose = FALSE, count = NULL, start = NULL) {
  params <- list(stream, blocks, verbose)
  if (!is.null(count)) params <- c(params, list(as.integer(count)))
  if (!is.null(start)) params <- c(params, list(as.integer(start)))
  
  res <- mc_rpc(conn, "liststreamblockitems", params)
  rpc_res_to_df(res)
}
