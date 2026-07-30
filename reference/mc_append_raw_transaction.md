# Add inputs and outputs to a raw transaction

Appends additional inputs and outputs to an existing raw transaction.
This is useful for building multi‑party transactions or adding extra
components after creation.

## Usage

``` r
mc_append_raw_transaction(conn, tx_hex, inputs = list(), outputs = list())
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- tx_hex:

  Character string. The raw transaction hex to which inputs/outputs are
  added.

- inputs:

  A list of input objects (each with `txid` and `vout`).

- outputs:

  A named list of outputs (mapping addresses to amounts).

## Value

A character string containing the updated raw transaction hex.

## See also

[`mc_create_raw_transaction`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_transaction.md),
[`mc_append_raw_change`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_change.md)

Other raw transactions:
[`mc_append_raw_change()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_change.md),
[`mc_append_raw_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_data.md),
[`mc_create_raw_send_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_send_from.md),
[`mc_create_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_transaction.md),
[`mc_decode_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_decode_raw_transaction.md),
[`mc_send_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_raw_transaction.md),
[`mc_sign_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_raw_transaction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Add another input and output
new_input <- list(list(txid = "def...", vout = 1))
new_output <- list("1C..." = 0.2)
updated_hex <- mc_append_raw_transaction(conn, tx_hex,
                                         inputs = new_input,
                                         outputs = new_output)
} # }
```
