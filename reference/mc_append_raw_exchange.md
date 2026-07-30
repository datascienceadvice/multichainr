# Add to a raw atomic exchange transaction

Appends a new input–output pair to a partially constructed atomic
exchange transaction. This function is used when multiple parties are
contributing to the exchange, each adding their own locked output and
specifying what they want in return.

## Usage

``` r
mc_append_raw_exchange(conn, tx_hex, txid, vout, amounts)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- tx_hex:

  Character string. Hexadecimal representation of the partial exchange
  transaction.

- txid:

  Character string. Transaction ID of the output being added to the
  offer.

- vout:

  Integer. Output index (vout) of the transaction being added.

- amounts:

  A list specifying the assets or native currency asked for in exchange
  for this addition. Format: `list(asset_name = quantity, ...)` or a
  numeric value for native currency.

## Value

A list with two elements:

- hex:

  The new partial transaction hex string.

- complete:

  Logical; `TRUE` if the exchange is now complete.

## See also

[`mc_create_raw_exchange`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_exchange.md),
[`mc_complete_raw_exchange`](https://datascienceadvice.github.io/multichainr/reference/mc_complete_raw_exchange.md)

Other atomic exchange:
[`mc_complete_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_complete_raw_exchange.md),
[`mc_create_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_exchange.md),
[`mc_decode_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_decode_raw_exchange.md),
[`mc_disable_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_disable_raw_transaction.md),
[`mc_prepare_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent.md),
[`mc_prepare_lock_unspent_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Assume 'partial_hex' is a partial exchange from a previous step
new <- mc_append_raw_exchange(conn, partial_hex,
                              txid = "abc...", vout = 0,
                              amounts = list(myasset = 10))
} # }
```
