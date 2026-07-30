# Get counts of blockchain entities

Returns the total number of various objects in the blockchain, such as
addresses, transactions, assets, streams, and permissions.

## Usage

``` r
mc_get_chain_totals(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

A list with counts, typically containing:

- addresses:

  Number of addresses.

- transactions:

  Number of transactions.

- assets:

  Number of assets.

- streams:

  Number of streams.

- permissions:

  Number of permission entries.

## See also

[`mc_get_blockchain_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_blockchain_info.md)
for global blockchain stats.

Other blockchain information:
[`mc_get_block()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block.md),
[`mc_get_block_hash()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block_hash.md),
[`mc_get_blockchain_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_blockchain_info.md),
[`mc_get_last_block_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_last_block_info.md),
[`mc_list_blocks()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_blocks.md),
[`mc_list_miners()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_miners.md)

## Examples

``` r
if (FALSE) { # \dontrun{
totals <- mc_get_chain_totals(conn)
print(totals$transactions)
} # }
```
