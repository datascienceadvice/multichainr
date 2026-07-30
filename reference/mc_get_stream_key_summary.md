# Get summary of JSON objects in a stream for a specific key

Aggregates JSON objects published under a specific key, merging them
according to the specified mode.

## Usage

``` r
mc_get_stream_key_summary(conn, stream, key, mode = "jsonobjectmerge")
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- stream:

  Character string. Stream name, reference, or txid.

- key:

  Character string. The key to summarize.

- mode:

  Character string. Merge mode (default `"jsonobjectmerge"`). Other
  options include `"recursive"`, `"noupdate"`, etc.

## Value

A JSON object (list) representing the merged summary.

## See also

[`mc_get_stream_publisher_summary`](https://datascienceadvice.github.io/multichainr/reference/mc_get_stream_publisher_summary.md)

Other streams:
[`mc_create_stream()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream.md),
[`mc_create_stream_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream_from.md),
[`mc_get_stream_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_stream_info.md),
[`mc_get_stream_item()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_stream_item.md),
[`mc_get_stream_publisher_summary()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_stream_publisher_summary.md),
[`mc_list_stream_block_items()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_block_items.md),
[`mc_list_stream_items()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_items.md),
[`mc_list_stream_key_items()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_key_items.md),
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
summary <- mc_get_stream_key_summary(conn, "mystream", "key", mode = "jsonobjectmerge")
} # }
```
