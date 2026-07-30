# Get memory pool information

Returns information about the node's memory pool (mempool), which holds
unconfirmed transactions awaiting inclusion in a block.

## Usage

``` r
mc_get_mempool_info(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

A list with mempool statistics:

- size:

  Number of transactions in the mempool.

- bytes:

  Total size in bytes.

- usage:

  Memory usage.

## See also

[`mc_get_raw_mempool`](https://datascienceadvice.github.io/multichainr/reference/mc_get_raw_mempool.md)
for the list of transaction IDs.

Other mempool & transactions:
[`mc_get_raw_mempool()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_raw_mempool.md),
[`mc_get_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_raw_transaction.md),
[`mc_get_tx_out()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_tx_out.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mempool <- mc_get_mempool_info(conn)
print(paste("Pending transactions:", mempool$size))
} # }
```
