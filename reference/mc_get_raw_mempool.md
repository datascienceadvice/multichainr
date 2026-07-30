# Get list of transaction IDs in mempool

Returns a character vector of transaction IDs currently in the node's
memory pool.

## Usage

``` r
mc_get_raw_mempool(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

A character vector of transaction IDs (txids).

## See also

[`mc_get_mempool_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_mempool_info.md)
for mempool statistics.

Other mempool & transactions:
[`mc_get_mempool_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_mempool_info.md),
[`mc_get_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_raw_transaction.md),
[`mc_get_tx_out()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_tx_out.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pending <- mc_get_raw_mempool(conn)
length(pending)  # number of pending transactions
} # }
```
