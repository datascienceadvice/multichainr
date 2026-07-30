# Append data to a binary cache item

Appends data to an existing binary cache item. If `data = ""` (the
default), the RPC call returns the current size without adding new data.

## Usage

``` r
mc_append_binary_cache(conn, identifier, data = "")
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- identifier:

  Character string. The cache item identifier returned by
  [`mc_create_binary_cache`](https://datascienceadvice.github.io/multichainr/reference/mc_create_binary_cache.md).

- data:

  Data to append. Can be:

  - a character string of hex data,

  - a list with element `text` (will be converted to hex),

  - a list with element `json` (will be converted to JSON and then to
    hex),

  - `""` (default) returns the current size without appending.

## Value

Integer. The resulting size of the cache item in bytes after appending
(or the current size if `data = ""`).

## See also

Other binary cache:
[`mc_create_binary_cache()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_binary_cache.md),
[`mc_delete_binary_cache()`](https://datascienceadvice.github.io/multichainr/reference/mc_delete_binary_cache.md),
[`mc_txout_to_binary_cache()`](https://datascienceadvice.github.io/multichainr/reference/mc_txout_to_binary_cache.md)
