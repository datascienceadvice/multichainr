# Create a raw transaction

Creates a raw (unsigned) transaction from a list of inputs and outputs.
This is the first step in building a custom transaction before signing
and broadcasting.

## Usage

``` r
mc_create_raw_transaction(conn, inputs, outputs, data = list(), action = "")
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- inputs:

  A list of input objects, each containing:

  - `txid` – Transaction ID of the UTXO.

  - `vout` – Output index (vout) of the UTXO.

- outputs:

  A named list (or list of named lists) mapping addresses to amounts.
  Example: `list("address1" = 0.5, "address2" = list(asset = 10))`.

- data:

  Optional array of metadata. Each element can be a string or a list
  (which will be converted to JSON then hex). The data is embedded in
  the transaction outputs.

- action:

  Optional action string: `"lock"` (lock inputs), `"sign"` (sign the
  transaction), `"lock,sign"` (both), or `"send"` (sign and send).
  Default is `""` (just create).

## Value

A character string containing the raw transaction hex (if no action) or
a list with `hex` and `complete` if action includes signing.

## See also

[`mc_sign_raw_transaction`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_raw_transaction.md),
[`mc_send_raw_transaction`](https://datascienceadvice.github.io/multichainr/reference/mc_send_raw_transaction.md)

Other raw transactions:
[`mc_append_raw_change()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_change.md),
[`mc_append_raw_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_data.md),
[`mc_append_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_transaction.md),
[`mc_create_raw_send_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_send_from.md),
[`mc_decode_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_decode_raw_transaction.md),
[`mc_send_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_raw_transaction.md),
[`mc_sign_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_raw_transaction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Build a simple transaction
inputs <- list(list(txid = "abc...", vout = 0))
outputs <- list("1A..." = 1.0)
tx_hex <- mc_create_raw_transaction(conn, inputs, outputs)

# With metadata
tx_hex <- mc_create_raw_transaction(conn, inputs, outputs,
                                    data = list("Hello", list(key = "value")))
} # }
```
