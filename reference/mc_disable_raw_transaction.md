# Disable an offer of exchange

Invalidates a previously created partial exchange transaction,
preventing it from being completed. The transaction is replaced with a
disabling transaction that spends the locked output(s) back to the
original owner(s).

## Usage

``` r
mc_disable_raw_transaction(conn, tx_hex)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- tx_hex:

  Character string. Hexadecimal representation of the exchange
  transaction to disable.

## Value

Character string. Transaction ID of the disabling transaction.

## See also

[`mc_prepare_lock_unspent`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent.md)

Other atomic exchange:
[`mc_append_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_exchange.md),
[`mc_complete_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_complete_raw_exchange.md),
[`mc_create_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_exchange.md),
[`mc_decode_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_decode_raw_exchange.md),
[`mc_prepare_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent.md),
[`mc_prepare_lock_unspent_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# After creating an offer, but before completion, you may decide to cancel
disable_txid <- mc_disable_raw_transaction(conn, offer_hex)
} # }
```
