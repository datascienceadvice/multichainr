# Prepare an unspent output from a specific address

Similar to
[`mc_prepare_lock_unspent`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent.md),
but allows specifying the source address from which to lock the outputs.
This is useful when the node has multiple addresses and you want to
control which address's funds are used.

## Usage

``` r
mc_prepare_lock_unspent_from(conn, from_address, amounts, lock = TRUE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- from_address:

  Character string. The address that will provide the assets/native
  currency.

- amounts:

  A list specifying the assets or native currency to lock.

- lock:

  Logical. If `TRUE` (default), the output is locked for automatic
  spending.

## Value

A list with `txid` and `vout`.

## See also

[`mc_prepare_lock_unspent`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent.md)

Other atomic exchange:
[`mc_append_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_exchange.md),
[`mc_complete_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_complete_raw_exchange.md),
[`mc_create_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_exchange.md),
[`mc_decode_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_decode_raw_exchange.md),
[`mc_disable_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_disable_raw_transaction.md),
[`mc_prepare_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent.md)

## Examples

``` r
if (FALSE) { # \dontrun{
locked <- mc_prepare_lock_unspent_from(conn, "1A...",
                                       amounts = list(myasset = 5))
} # }
```
