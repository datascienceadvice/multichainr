# List items with a specific key

Returns all items in a stream that have a given key.

## Usage

``` r
mc_list_stream_key_items(
  conn,
  stream,
  key,
  verbose = FALSE,
  count = 10,
  start = NULL,
  local_ordering = FALSE
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- stream:

  Character string. Stream name, reference, or txid.

- key:

  Character string. The key to filter by.

- verbose:

  Logical. If `TRUE`, returns detailed information.

- count:

  Integer. Number of items to return (default 10).

- start:

  Optional integer. Offset for pagination.

- local_ordering:

  Logical. Use local ordering.

## Value

A data frame (via `rpc_res_to_df`) with items.

## See also

Other streams:
[`mc_create_stream()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream.md),
[`mc_create_stream_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream_from.md),
[`mc_get_stream_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_stream_info.md),
[`mc_get_stream_item()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_stream_item.md),
[`mc_get_stream_key_summary()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_stream_key_summary.md),
[`mc_get_stream_publisher_summary()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_stream_publisher_summary.md),
[`mc_list_stream_block_items()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_block_items.md),
[`mc_list_stream_items()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_items.md),
[`mc_list_stream_keys()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_keys.md),
[`mc_list_stream_publisher_items()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_publisher_items.md),
[`mc_list_stream_publishers()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_publishers.md),
[`mc_list_stream_query_items()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_query_items.md),
[`mc_list_stream_tx_items()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_tx_items.md),
[`mc_list_streams()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_streams.md),
[`mc_publish()`](https://datascienceadvice.github.io/multichainr/reference/mc_publish.md),
[`mc_publish_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_publish_from.md),
[`mc_publish_multi()`](https://datascienceadvice.github.io/multichainr/reference/mc_publish_multi.md),
[`mc_publish_multi_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_publish_multi_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
items <- mc_list_stream_key_items(conn, "mystream", "mykey")
} # }
```
