# Finalize an atomic exchange transaction

Completes a multi-party atomic exchange by adding the final input–output
pair. After this step, the transaction is fully built and ready to be
broadcast.

## Usage

``` r
mc_complete_raw_exchange(conn, tx_hex, txid, vout, amounts, data = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- tx_hex:

  Character string. Hexadecimal representation of the partial exchange
  transaction (should already contain all other parties' contributions).

- txid:

  Character string. Transaction ID of the completing output.

- vout:

  Integer. Output index (vout) of the completing transaction.

- amounts:

  A list specifying the assets or native currency for the final part of
  the exchange.

- data:

  Optional metadata. Can be a character string or a list (which will be
  converted to JSON and then to hex). This data is embedded in the
  transaction.

## Value

Character string. Raw transaction hex ready for sending via
[`mc_send_raw_transaction`](https://datascienceadvice.github.io/multichainr/reference/mc_send_raw_transaction.md).

## See also

[`mc_create_raw_exchange`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_exchange.md),
[`mc_append_raw_exchange`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_exchange.md)

Other atomic exchange:
[`mc_append_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_exchange.md),
[`mc_create_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_exchange.md),
[`mc_decode_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_decode_raw_exchange.md),
[`mc_disable_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_disable_raw_transaction.md),
[`mc_prepare_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent.md),
[`mc_prepare_lock_unspent_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
final <- mc_complete_raw_exchange(conn, partial_hex,
                                  txid = "def...", vout = 1,
                                  amounts = list(ETH = 5),
                                  data = "exchange complete")
} # }
```
