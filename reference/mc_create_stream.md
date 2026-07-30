# Create a new stream

Creates a new stream on the blockchain. Streams are ordered collections
of key‑value items that can be used for data storage, messaging, or
other applications. The stream can be open (anyone can write) or
restricted.

## Usage

``` r
mc_create_stream(conn, name, open = TRUE, custom_fields = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- name:

  Character string. Name of the stream (must be unique).

- open:

  Either a logical (TRUE for open stream, FALSE for restricted) or a
  list of parameters, e.g., `list(restrict = "write")`. For open
  streams, any address with `send` permission can publish. For
  restricted, only addresses with `write` permission on the stream can
  publish.

- custom_fields:

  Optional list of custom fields (e.g., `list(field1 = "value")`).

## Value

A character string containing the transaction ID (txid) of the stream
creation.

## See also

[`mc_create_stream_from`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream_from.md)
to specify the creator address,
[`mc_get_stream_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_stream_info.md)
to query stream details.

Other streams:
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
[`mc_publish_multi()`](https://datascienceadvice.github.io/multichainr/reference/mc_publish_multi.md),
[`mc_publish_multi_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_publish_multi_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create an open stream
txid <- mc_create_stream(conn, "mystream", open = TRUE)

# Create a restricted stream with custom fields
txid <- mc_create_stream(conn, "private", open = list(restrict = "write"),
                         custom_fields = list(owner = "admin"))
} # }
```
