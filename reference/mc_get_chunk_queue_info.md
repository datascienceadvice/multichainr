# Get information about off-chain chunk queue

Returns details about the node's off‑chain chunk queue, which handles
the transmission of large data items that are split into chunks.

## Usage

``` r
mc_get_chunk_queue_info(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

A list containing:

- chunk_count:

  Number of chunks currently queued.

- byte_count:

  Total size in bytes of queued chunks.

- ...:

  Other queue statistics.

## See also

[`mc_get_chunk_queue_totals`](https://datascienceadvice.github.io/multichainr/reference/mc_get_chunk_queue_totals.md)
for cumulative statistics.

Other off-chain data:
[`mc_get_chunk_queue_totals()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_chunk_queue_totals.md)

## Examples

``` r
if (FALSE) { # \dontrun{
queue_info <- mc_get_chunk_queue_info(conn)
cat("Chunks pending:", queue_info$chunk_count)
} # }
```
