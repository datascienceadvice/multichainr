# Create a new binary cache item

Creates an empty item (file) in the node's binary cache and returns its
unique identifier. Binary cache items are temporary storage for binary
data that can be used in transactions or passed between nodes.

## Usage

``` r
mc_create_binary_cache(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

A character string identifier (filename) for the newly created cache
item.

## See also

[`mc_append_binary_cache`](https://datascienceadvice.github.io/multichainr/reference/mc_append_binary_cache.md)
to add data,
[`mc_delete_binary_cache`](https://datascienceadvice.github.io/multichainr/reference/mc_delete_binary_cache.md)
to remove.

Other binary cache:
[`mc_append_binary_cache()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_binary_cache.md),
[`mc_delete_binary_cache()`](https://datascienceadvice.github.io/multichainr/reference/mc_delete_binary_cache.md),
[`mc_txout_to_binary_cache()`](https://datascienceadvice.github.io/multichainr/reference/mc_txout_to_binary_cache.md)

## Examples

``` r
if (FALSE) { # \dontrun{
id <- mc_create_binary_cache(conn)
} # }
```
