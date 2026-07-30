# Decode a raw exchange transaction

Parses a raw atomic exchange transaction (partial or complete) and
returns a human‑readable representation of its structure, including the
involved inputs, outputs, and the assets being exchanged.

## Usage

``` r
mc_decode_raw_exchange(conn, tx_hex, verbose = FALSE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- tx_hex:

  Character string. Hexadecimal representation of the exchange
  transaction.

- verbose:

  Logical. If `TRUE`, lists all individual stages (contributions) of the
  exchange. Default is `FALSE`.

## Value

A list (or a data frame if `verbose`) containing the decoded exchange
details.

## See also

[`mc_create_raw_exchange`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_exchange.md)

Other atomic exchange:
[`mc_append_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_exchange.md),
[`mc_complete_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_complete_raw_exchange.md),
[`mc_create_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_exchange.md),
[`mc_disable_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_disable_raw_transaction.md),
[`mc_prepare_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent.md),
[`mc_prepare_lock_unspent_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_prepare_lock_unspent_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
decoded <- mc_decode_raw_exchange(conn, my_tx_hex)
print(decoded)
} # }
```
