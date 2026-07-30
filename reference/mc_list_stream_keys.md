# List unique keys in a stream

Returns the set of distinct keys used in a stream, with optional
pagination.

## Usage

``` r
mc_list_stream_keys(
  conn,
  stream,
  keys = "*",
  verbose = FALSE,
  count = NULL,
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

- keys:

  Character vector of keys to filter (default `"*"` for all).

- verbose:

  Logical. If `TRUE`, returns additional metadata per key.

- count:

  Optional integer. Number of keys to return.

- start:

  Optional integer. Offset.

- local_ordering:

  Logical. Use local ordering.

## Value

A data frame (via `rpc_res_to_df`) with key information.

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
[`mc_list_stream_key_items()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_key_items.md),
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
keys <- mc_list_stream_keys(conn, "mystream")
} # }
```
