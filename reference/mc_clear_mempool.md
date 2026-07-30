# Clear the node's memory pool

Removes all unconfirmed transactions from the node's memory pool
(mempool). This function is typically used after pausing incoming and
mining tasks to reset the mempool state. It requires the node to be
paused first with `mc_pause(conn, "incoming,mining")`.

## Usage

``` r
mc_clear_mempool(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

Invisibly returns the RPC result (typically `NULL`) on success.

## See also

[`mc_pause`](https://datascienceadvice.github.io/multichainr/reference/mc_pause.md),
[`mc_resume`](https://datascienceadvice.github.io/multichainr/reference/mc_resume.md),
[`mc_get_mempool_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_mempool_info.md)
to inspect mempool state.

## Examples

``` r
if (FALSE) { # \dontrun{
# Pause the node before clearing the mempool
mc_pause(conn, "incoming,mining")
mc_clear_mempool(conn)
mc_resume(conn, "incoming,mining")
} # }
```
