# Get a raw transaction from the blockchain

Retrieves a transaction from the blockchain by its transaction ID. If
`verbose = TRUE`, returns a detailed decoded transaction; if `FALSE`
(default), returns the raw transaction in hexadecimal.

## Usage

``` r
mc_get_raw_transaction(conn, txid, verbose = FALSE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- txid:

  Character string. Transaction ID.

- verbose:

  Logical. If `TRUE`, returns a parsed transaction object; if `FALSE`,
  returns the raw transaction hex. Default is `FALSE`.

## Value

If `verbose = FALSE`, a character string (hex). If `verbose = TRUE`, a
list with transaction details.

## See also

[`mc_get_tx_out`](https://datascienceadvice.github.io/multichainr/reference/mc_get_tx_out.md)
to inspect a specific output.

Other mempool & transactions:
[`mc_get_mempool_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_mempool_info.md),
[`mc_get_raw_mempool()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_raw_mempool.md),
[`mc_get_tx_out()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_tx_out.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get raw hex of a transaction
raw <- mc_get_raw_transaction(conn, "abc...")

# Get decoded transaction
tx <- mc_get_raw_transaction(conn, "abc...", verbose = TRUE)
} # }
```
