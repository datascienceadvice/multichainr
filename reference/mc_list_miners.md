# List miners and their status

Returns information about nodes that are mining (or have mining
permission) on the blockchain.

## Usage

``` r
mc_list_miners(conn, verbose = FALSE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- verbose:

  Logical. If `TRUE`, returns additional details about each miner (e.g.,
  mining status, last block mined). Default is `FALSE`.

## Value

A data frame (via `rpc_res_to_df`) with miner information. Typical
columns: `address`, `status`, `lastblocktime`, etc.

## See also

Other blockchain information:
[`mc_get_block()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block.md),
[`mc_get_block_hash()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block_hash.md),
[`mc_get_blockchain_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_blockchain_info.md),
[`mc_get_chain_totals()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_chain_totals.md),
[`mc_get_last_block_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_last_block_info.md),
[`mc_list_blocks()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_blocks.md)

## Examples

``` r
if (FALSE) { # \dontrun{
miners <- mc_list_miners(conn)
print(miners)
} # }
```
