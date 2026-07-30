# Decode a raw transaction hex

Parses a raw transaction hex string into a human‑readable structure,
showing inputs, outputs, amounts, metadata, and other transaction
details.

## Usage

``` r
mc_decode_raw_transaction(conn, tx_hex)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- tx_hex:

  Character string. The raw transaction hex to decode.

## Value

A list with decoded transaction information (inputs, outputs, etc.).

## See also

[`mc_get_raw_transaction`](https://datascienceadvice.github.io/multichainr/reference/mc_get_raw_transaction.md)

Other raw transactions:
[`mc_append_raw_change()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_change.md),
[`mc_append_raw_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_data.md),
[`mc_append_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_transaction.md),
[`mc_create_raw_send_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_send_from.md),
[`mc_create_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_transaction.md),
[`mc_send_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_raw_transaction.md),
[`mc_sign_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_raw_transaction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
decoded <- mc_decode_raw_transaction(conn, tx_hex)
print(decoded$vin)
} # }
```
