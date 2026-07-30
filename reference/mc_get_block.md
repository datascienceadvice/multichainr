# Get block information

Retrieves detailed information about a specific block. The block can be
identified either by its hash (string) or height (integer). The
verbosity level controls how much detail is returned.

## Usage

``` r
mc_get_block(conn, hash_or_height, verbose = 1)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- hash_or_height:

  Either a character string (block hash) or an integer (block height).

- verbose:

  Integer. Verbosity level from 0 to 4. Default is `1`.

  - `0`: returns only the block hash as a string.

  - `1`: returns a list with basic block information.

  - `2-4`: include additional details (transactions, etc.).

## Value

Depends on `verbose`:

- If `verbose = 0`: a character string with the block hash.

- If `verbose >= 1`: a list containing block details (height, time,
  transactions, etc.).

## See also

[`mc_get_block_hash`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block_hash.md)
to obtain a block hash from height,
[`mc_list_blocks`](https://datascienceadvice.github.io/multichainr/reference/mc_list_blocks.md)
to list multiple blocks.

Other blockchain information:
[`mc_get_block_hash()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block_hash.md),
[`mc_get_blockchain_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_blockchain_info.md),
[`mc_get_chain_totals()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_chain_totals.md),
[`mc_get_last_block_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_last_block_info.md),
[`mc_list_blocks()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_blocks.md),
[`mc_list_miners()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_miners.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get block by height
block <- mc_get_block(conn, 123456)

# Get block by hash with full transaction details
block <- mc_get_block(conn, "0000...", verbose = 2)
} # }
```
