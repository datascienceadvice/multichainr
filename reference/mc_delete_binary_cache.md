# Delete an item from the binary cache

Removes a previously created binary cache item. Once deleted, the
identifier becomes invalid and cannot be used further.

## Usage

``` r
mc_delete_binary_cache(conn, identifier)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- identifier:

  Character string. The cache item identifier to remove.

## Value

Invisibly returns `NULL` on success; throws an error if the item does
not exist or cannot be deleted.

## See also

[`mc_create_binary_cache`](https://datascienceadvice.github.io/multichainr/reference/mc_create_binary_cache.md),
[`mc_append_binary_cache`](https://datascienceadvice.github.io/multichainr/reference/mc_append_binary_cache.md)

Other binary cache:
[`mc_append_binary_cache()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_binary_cache.md),
[`mc_create_binary_cache()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_binary_cache.md),
[`mc_txout_to_binary_cache()`](https://datascienceadvice.github.io/multichainr/reference/mc_txout_to_binary_cache.md)

## Examples

``` r
if (FALSE) { # \dontrun{
id <- mc_create_binary_cache(conn)
# ... use the cache item ...
mc_delete_binary_cache(conn, id)
} # }
```
