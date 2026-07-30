# List information about specific blocks

Retrieves information about one or more blocks. The `blocks` parameter
can be a single block height/hash, a range (e.g., `"100-200"`), or `-%d`
to list the most recent blocks.

## Usage

``` r
mc_list_blocks(conn, blocks, verbose = FALSE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- blocks:

  Specification of which blocks to list. Can be:

  - a single block height (integer) or hash (string),

  - a range string like `"100-200"`,

  - a negative integer `-n` to list the `n` most recent blocks.

- verbose:

  Logical. If `TRUE`, returns detailed block information. Default is
  `FALSE`.

## Value

A data frame (converted via `rpc_res_to_df`) with one row per block.

## See also

[`mc_get_block`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block.md)
for a single block.

Other blockchain information:
[`mc_get_block()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block.md),
[`mc_get_block_hash()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block_hash.md),
[`mc_get_blockchain_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_blockchain_info.md),
[`mc_get_chain_totals()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_chain_totals.md),
[`mc_get_last_block_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_last_block_info.md),
[`mc_list_miners()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_miners.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# List the last 5 blocks
last5 <- mc_list_blocks(conn, -5)

# List blocks 100 to 105
range <- mc_list_blocks(conn, "100-105", verbose = TRUE)
} # }
```
