# Add a change output to a raw transaction

Appends a change output to a raw transaction. This is useful when the
transaction inputs exceed the required output amounts; the change is
sent back to the specified address.

## Usage

``` r
mc_append_raw_change(conn, tx_hex, address, native_fee = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- tx_hex:

  Character string. The raw transaction hex to which change is added.

- address:

  Character string. The address that will receive the change.

- native_fee:

  Optional numeric. Native currency fee to be deducted from the change.
  If provided, the change output is reduced accordingly.

## Value

A character string containing the updated raw transaction hex.

## See also

[`mc_append_raw_data`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_data.md),
[`mc_append_raw_transaction`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_transaction.md)

Other raw transactions:
[`mc_append_raw_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_data.md),
[`mc_append_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_transaction.md),
[`mc_create_raw_send_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_send_from.md),
[`mc_create_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_transaction.md),
[`mc_decode_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_decode_raw_transaction.md),
[`mc_send_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_raw_transaction.md),
[`mc_sign_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_raw_transaction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Add change to a raw transaction
updated_hex <- mc_append_raw_change(conn, tx_hex, "1A...")
} # }
```
