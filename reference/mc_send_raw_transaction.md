# Send a signed raw transaction to the network

Broadcasts a signed raw transaction to the blockchain network. The
transaction must be complete (all inputs signed) before sending.

## Usage

``` r
mc_send_raw_transaction(conn, tx_hex)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- tx_hex:

  Character string. The signed raw transaction hex to send.

## Value

A character string containing the transaction ID (txid).

## See also

[`mc_sign_raw_transaction`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_raw_transaction.md),
[`mc_create_raw_transaction`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_transaction.md)

Other raw transactions:
[`mc_append_raw_change()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_change.md),
[`mc_append_raw_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_data.md),
[`mc_append_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_transaction.md),
[`mc_create_raw_send_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_send_from.md),
[`mc_create_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_transaction.md),
[`mc_decode_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_decode_raw_transaction.md),
[`mc_sign_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_raw_transaction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create, sign, and send a transaction
tx_hex <- mc_create_raw_transaction(conn, inputs, outputs)
signed <- mc_sign_raw_transaction(conn, tx_hex)
if (signed$complete) {
  txid <- mc_send_raw_transaction(conn, signed$hex)
}
} # }
```
