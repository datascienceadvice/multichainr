# Get details about an unspent transaction output

Returns information about a specific unspent transaction output (UTXO).
This can be useful for building transactions or checking balances.

## Usage

``` r
mc_get_tx_out(conn, txid, vout, unconfirmed = FALSE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- txid:

  Character string. Transaction ID.

- vout:

  Integer. Output index.

- unconfirmed:

  Logical. If `TRUE`, includes unconfirmed transactions (from the
  mempool). Default is `FALSE`.

## Value

A list with output details, including:

- bestblock:

  Hash of the best block.

- confirmations:

  Number of confirmations.

- value:

  Amount (in native currency).

- scriptPubKey:

  Output script details.

Returns `NULL` if the output does not exist or is spent.

## See also

[`mc_get_raw_transaction`](https://datascienceadvice.github.io/multichainr/reference/mc_get_raw_transaction.md)
for full transaction details.

Other mempool & transactions:
[`mc_get_mempool_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_mempool_info.md),
[`mc_get_raw_mempool()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_raw_mempool.md),
[`mc_get_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_raw_transaction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Check an output from a confirmed transaction
out <- mc_get_tx_out(conn, "abc...", vout = 0)
if (!is.null(out)) print(out$value)
} # }
```
