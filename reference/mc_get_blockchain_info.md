# Get general blockchain information

Returns global information about the blockchain, such as the current
block height, chain name, protocol version, difficulty, and consensus
status.

## Usage

``` r
mc_get_blockchain_info(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

A list with blockchain metadata. Typical fields:

- chain:

  Name of the chain.

- blocks:

  Current block height.

- headers:

  Number of block headers.

- bestblockhash:

  Hash of the most recent block.

- difficulty:

  Current mining difficulty.

- chainwork:

  Total work in the chain.

## See also

[`mc_get_chain_totals`](https://datascienceadvice.github.io/multichainr/reference/mc_get_chain_totals.md)
for counts of entities,
[`mc_get_last_block_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_last_block_info.md)
for the most recent block.

Other blockchain information:
[`mc_get_block()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block.md),
[`mc_get_block_hash()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block_hash.md),
[`mc_get_chain_totals()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_chain_totals.md),
[`mc_get_last_block_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_last_block_info.md),
[`mc_list_blocks()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_blocks.md),
[`mc_list_miners()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_miners.md)

## Examples

``` r
if (FALSE) { # \dontrun{
info <- mc_get_blockchain_info(conn)
print(info$blocks)
} # }
```
