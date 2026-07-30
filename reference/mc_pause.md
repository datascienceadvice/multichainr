# Pause specified node tasks

Temporarily suspends certain node operations without shutting down the
node. Tasks that can be paused include mining, incoming connections, and
off‑chain data handling.

## Usage

``` r
mc_pause(conn, tasks)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- tasks:

  A character vector or a comma‑separated string of tasks to pause.
  Valid task names are:

  - `"mining"` – stop mining new blocks.

  - `"incoming"` – stop accepting incoming connections.

  - `"offchain"` – stop processing off-chain data.

  Multiple tasks can be specified, e.g., `c("mining", "incoming")`.

## Value

Invisibly returns the RPC result (typically `NULL`) on success.

## See also

[`mc_resume`](https://datascienceadvice.github.io/multichainr/reference/mc_resume.md)
to restart paused tasks,
[`mc_clear_mempool`](https://datascienceadvice.github.io/multichainr/reference/mc_clear_mempool.md)
for use after pausing.

Other node control:
[`mc_resume()`](https://datascienceadvice.github.io/multichainr/reference/mc_resume.md),
[`mc_set_last_block()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_last_block.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Pause mining only
mc_pause(conn, "mining")

# Pause both incoming connections and mining
mc_pause(conn, c("incoming", "mining"))
} # }
```
