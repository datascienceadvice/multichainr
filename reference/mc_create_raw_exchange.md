# Create a new atomic exchange transaction

Initialises a partial atomic exchange transaction by specifying the
first locked output and the desired assets/currency in return. This is
the first step in constructing a multi‑party atomic exchange.

## Usage

``` r
mc_create_raw_exchange(conn, txid, vout, amounts)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- txid:

  Character string. Transaction ID of the locked output (obtained via
  [`mc_prepare_lock_unspent`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent.md)).

- vout:

  Integer. Output index (vout) of the locked output.

- amounts:

  A list specifying the assets or native currency asked for in exchange.
  Format: `list(asset_name = quantity, ...)` or a numeric value for
  native currency.

## Value

Character string. Raw partial transaction in hexadecimal.

## See also

[`mc_prepare_lock_unspent`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent.md),
[`mc_append_raw_exchange`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_exchange.md)

Other atomic exchange:
[`mc_append_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_exchange.md),
[`mc_complete_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_complete_raw_exchange.md),
[`mc_decode_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_decode_raw_exchange.md),
[`mc_disable_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_disable_raw_transaction.md),
[`mc_prepare_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent.md),
[`mc_prepare_lock_unspent_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# First, lock some output
locked <- mc_prepare_lock_unspent(conn, amounts = list(myasset = 10))
# Create the exchange offer
offer <- mc_create_raw_exchange(conn, locked$txid, locked$vout,
                                amounts = list(otherasset = 5))
} # }
```
