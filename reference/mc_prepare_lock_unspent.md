# Prepare an unspent transaction output for exchange

Locks one or more unspent outputs (assets or native currency) to be used
as part of an atomic exchange. The locked output is prepared in a way
that it can only be spent as part of an atomic exchange transaction.

## Usage

``` r
mc_prepare_lock_unspent(conn, amounts, lock = TRUE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- amounts:

  A list specifying the assets or native currency to lock. Format:
  `list(asset_name = quantity, ...)` or a numeric value for native
  currency.

- lock:

  Logical. If `TRUE` (default), the output is locked for automatic
  spending (i.e., it will be used only in the exchange). If `FALSE`, the
  output is simply prepared but not locked.

## Value

A list with two elements:

- txid:

  Transaction ID of the prepared/locked output.

- vout:

  Output index.

## See also

[`mc_prepare_lock_unspent_from`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent_from.md),
[`mc_create_raw_exchange`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_exchange.md)

Other atomic exchange:
[`mc_append_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_exchange.md),
[`mc_complete_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_complete_raw_exchange.md),
[`mc_create_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_exchange.md),
[`mc_decode_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_decode_raw_exchange.md),
[`mc_disable_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_disable_raw_transaction.md),
[`mc_prepare_lock_unspent_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Lock 10 units of 'myasset' and 0.5 native currency
locked <- mc_prepare_lock_unspent(conn, amounts = list(myasset = 10, 0.5))
# Now use locked$txid and locked$vout in an exchange
} # }
```
