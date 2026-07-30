# Publish multiple items to a stream in one transaction

Publishes several key‑value items in a single transaction. This is more
efficient than publishing each item separately.

## Usage

``` r
mc_publish_multi(conn, stream, items, options = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- stream:

  Character string. Default stream for all items (if not overridden per
  item).

- items:

  A list of item objects, each containing:

  - `key` or `keys` – the key(s) for the item.

  - `data` – the data to publish.

  - (optional) `stream` – override the default stream.

- options:

  Optional default `"offchain"` string.

## Value

A character string containing the transaction ID.

## See also

[`mc_publish_multi_from`](https://datascienceadvice.github.io/multichainr/reference/mc_publish_multi_from.md)
for specifying the sender.

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
[`mc_list_stream_keys()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_keys.md),
[`mc_list_stream_publisher_items()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_publisher_items.md),
[`mc_list_stream_publishers()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_publishers.md),
[`mc_list_stream_query_items()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_query_items.md),
[`mc_list_stream_tx_items()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_tx_items.md),
[`mc_list_streams()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_streams.md),
[`mc_publish()`](https://datascienceadvice.github.io/multichainr/reference/mc_publish.md),
[`mc_publish_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_publish_from.md),
[`mc_publish_multi_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_publish_multi_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
items <- list(
  list(key = "k1", data = list(text = "item1")),
  list(key = "k2", data = list(json = list(value = 2)))
)
txid <- mc_publish_multi(conn, "mystream", items)
} # }
```
