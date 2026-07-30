# Get cumulative statistics on off-chain chunk requests

Returns total counts of chunk deliveries, failures, and timeouts since
the node started.

## Usage

``` r
mc_get_chunk_queue_totals(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

A list with cumulative statistics:

- delivered:

  Number of chunks successfully delivered.

- undelivered:

  Number of chunks not yet delivered.

- timeouts:

  Number of delivery timeouts.

- ...:

  Other totals.

## See also

[`mc_get_chunk_queue_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_chunk_queue_info.md)
for current queue state.

Other off-chain data:
[`mc_get_chunk_queue_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_chunk_queue_info.md)

## Examples

``` r
if (FALSE) { # \dontrun{
totals <- mc_get_chunk_queue_totals(conn)
print(totals)
} # }
```
