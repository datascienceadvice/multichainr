# Rewind the node's active chain

Moves the node's active chain to a previous block, effectively rolling
back the blockchain state. This is a powerful operation typically used
for testing or recovery. The node must be paused with
`mc_pause(conn, "incoming,mining")` before calling.

## Usage

``` r
mc_set_last_block(conn, hash_or_height)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- hash_or_height:

  Either a block hash (character string) or a block height (integer) to
  rewind to.

## Value

Character string. The hash of the last block after the rewind.

## See also

[`mc_pause`](https://datascienceadvice.github.io/multichainr/reference/mc_pause.md),
[`mc_resume`](https://datascienceadvice.github.io/multichainr/reference/mc_resume.md),
[`mc_get_block`](https://datascienceadvice.github.io/multichainr/reference/mc_get_block.md)
to inspect blocks.

Other node control:
[`mc_pause()`](https://datascienceadvice.github.io/multichainr/reference/mc_pause.md),
[`mc_resume()`](https://datascienceadvice.github.io/multichainr/reference/mc_resume.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Pause the node first
mc_pause(conn, "incoming,mining")
# Rewind to block height 100
last_hash <- mc_set_last_block(conn, 100)
# Resume after rewind
mc_resume(conn, "incoming,mining")
} # }
```
