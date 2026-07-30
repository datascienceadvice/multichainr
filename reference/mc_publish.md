# Publish an item to a stream

Writes a key‑value item to a stream. The item is stored on the
blockchain (or optionally off‑chain) and can be retrieved by its key.

## Usage

``` r
mc_publish(conn, stream, keys, data, options = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- stream:

  Character string. Stream name, reference, or creation txid.

- keys:

  A single key (string) or a vector of keys (for multi‑key items).

- data:

  Data to publish. Can be a hex string, or a list with `text` (will be
  hex‑encoded) or `json` (will be converted to JSON then hex).

- options:

  Optional character string. Use `"offchain"` to publish as an off‑chain
  item (requires off‑chain capability on the blockchain).

## Value

A character string containing the transaction ID of the published item.

## See also

[`mc_publish_from`](https://datascienceadvice.github.io/multichainr/reference/mc_publish_from.md)
to specify publisher address,
[`mc_publish_multi`](https://datascienceadvice.github.io/multichainr/reference/mc_publish_multi.md)
for multiple items.

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
[`mc_publish_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_publish_from.md),
[`mc_publish_multi()`](https://datascienceadvice.github.io/multichainr/reference/mc_publish_multi.md),
[`mc_publish_multi_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_publish_multi_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Publish with a text key and simple text data
mc_publish(conn, "mystream", "greeting", list(text = "Hello world!"))

# Publish with JSON data
mc_publish(conn, "mystream", "data", list(json = list(a = 1, b = 2)))

# Publish off‑chain
mc_publish(conn, "mystream", "large", list(text = "big data"), options = "offchain")
} # }
```
