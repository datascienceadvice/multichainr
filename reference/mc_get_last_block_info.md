# Get information about the last block

Retrieves details of the most recent block, optionally skipping back by
a number of blocks.

## Usage

``` r
mc_get_last_block_info(conn, skip = 0)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- skip:

  Integer. Number of blocks to skip back from the tip. `skip = 0`
  returns the latest block, `skip = 1` returns the previous block, etc.
  Default is `0`.

## Value

A list with block information (similar to
[`mc_get_block`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block.md)).

## See also

[`mc_get_block`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block.md)
for general block retrieval.

Other blockchain information:
[`mc_get_block()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block.md),
[`mc_get_block_hash()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block_hash.md),
[`mc_get_blockchain_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_blockchain_info.md),
[`mc_get_chain_totals()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_chain_totals.md),
[`mc_list_blocks()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_blocks.md),
[`mc_list_miners()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_miners.md)

## Examples

``` r
if (FALSE) { # \dontrun{
latest <- mc_get_last_block_info(conn)
previous <- mc_get_last_block_info(conn, skip = 1)
} # }
```
