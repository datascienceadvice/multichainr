# Add metadata to a raw transaction

Appends arbitrary data (metadata) to a raw transaction. The data is
embedded in an output with zero native value.

## Usage

``` r
mc_append_raw_data(conn, tx_hex, data)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- tx_hex:

  Character string. The raw transaction hex.

- data:

  Data to embed. Can be a string or a list (converted to JSON then hex).

## Value

A character string containing the updated raw transaction hex.

## See also

[`mc_append_raw_change`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_change.md),
[`mc_append_raw_transaction`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_transaction.md)

Other raw transactions:
[`mc_append_raw_change()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_change.md),
[`mc_append_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_transaction.md),
[`mc_create_raw_send_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_send_from.md),
[`mc_create_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_transaction.md),
[`mc_decode_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_decode_raw_transaction.md),
[`mc_send_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_raw_transaction.md),
[`mc_sign_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_raw_transaction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Add a text note
updated_hex <- mc_append_raw_data(conn, tx_hex, "This is a note.")

# Add JSON metadata
updated_hex <- mc_append_raw_data(conn, tx_hex, list(tag = "invoice", id = 123))
} # }
```
