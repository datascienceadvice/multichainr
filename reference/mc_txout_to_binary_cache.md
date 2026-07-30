# Extract transaction output data to binary cache

Copies data directly from a blockchain transaction output into a binary
cache item. This is efficient for retrieving binary data stored in a
transaction (e.g., via
[`mc_publish`](https://datascienceadvice.github.io/multichainr/reference/mc_publish.md))
without having to decode it in R.

## Usage

``` r
mc_txout_to_binary_cache(
  conn,
  identifier,
  txid,
  vout,
  count_bytes = NULL,
  start_byte = 0
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- identifier:

  Character string. Target cache item identifier. The cache item must be
  empty (created but not yet written to).

- txid:

  Character string. Transaction ID containing the output.

- vout:

  Integer. Output index (vout) of the transaction to extract.

- count_bytes:

  Integer (optional). Number of bytes to extract. If `NULL` (default),
  the entire output data is copied.

- start_byte:

  Integer (optional). Byte offset from which to start copying. Default
  is `0` (beginning of the output data).

## Value

Integer. The resulting size of the cache item after extraction.

## See also

[`mc_create_binary_cache`](https://datascienceadvice.github.io/multichainr/reference/mc_create_binary_cache.md),
[`mc_append_binary_cache`](https://datascienceadvice.github.io/multichainr/reference/mc_append_binary_cache.md)

Other binary cache:
[`mc_append_binary_cache()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_binary_cache.md),
[`mc_create_binary_cache()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_binary_cache.md),
[`mc_delete_binary_cache()`](https://datascienceadvice.github.io/multichainr/reference/mc_delete_binary_cache.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create an empty cache item
id <- mc_create_binary_cache(conn)

# Copy the entire data from a transaction output
size <- mc_txout_to_binary_cache(conn, id, txid = "abc...", vout = 0)

# Copy only the first 100 bytes
size <- mc_txout_to_binary_cache(conn, id, txid = "abc...", vout = 0,
                                 count_bytes = 100)
} # }
```
