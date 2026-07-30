# Resume specified node tasks

Restarts node tasks that were previously paused with
[`mc_pause`](https://datascienceadvice.github.io/multichainr/reference/mc_pause.md).

## Usage

``` r
mc_resume(conn, tasks)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- tasks:

  A character vector or a comma‑separated string of tasks to resume.
  Valid task names: `"mining"`, `"incoming"`, `"offchain"`.

## Value

Invisibly returns the RPC result (typically `NULL`) on success.

## See also

[`mc_pause`](https://datascienceadvice.github.io/multichainr/reference/mc_pause.md)
to pause tasks.

Other node control:
[`mc_pause()`](https://datascienceadvice.github.io/multichainr/reference/mc_pause.md),
[`mc_set_last_block()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_last_block.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Resume mining after a pause
mc_resume(conn, "mining")

# Resume all paused tasks
mc_resume(conn, c("mining", "incoming", "offchain"))
} # }
```
