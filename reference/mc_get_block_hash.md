# Get block hash by height

Returns the hash of a block at a given height.

## Usage

``` r
mc_get_block_hash(conn, height)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- height:

  Integer. The block height (0 for genesis block).

## Value

A character string containing the block hash.

## See also

[`mc_get_block`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block.md)
to retrieve block details.

Other blockchain information:
[`mc_get_block()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block.md),
[`mc_get_blockchain_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_blockchain_info.md),
[`mc_get_chain_totals()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_chain_totals.md),
[`mc_get_last_block_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_last_block_info.md),
[`mc_list_blocks()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_blocks.md),
[`mc_list_miners()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_miners.md)

## Examples

``` r
if (FALSE) { # \dontrun{
hash <- mc_get_block_hash(conn, 0)  # genesis block hash
} # }
```
