# List libraries on the blockchain

Returns a list of libraries with optional filtering and verbosity.

## Usage

``` r
mc_list_libraries(conn, libraries = "*", verbose = FALSE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- libraries:

  Character vector of library names/IDs, or `"*"` (default) for all
  libraries.

- verbose:

  Logical. If `TRUE`, returns detailed information.

## Value

A data frame (via `rpc_res_to_df`) with library information.

## See also

Other libraries:
[`mc_add_library_update()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_library_update.md),
[`mc_add_library_update_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_library_update_from.md),
[`mc_create_library()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_library.md),
[`mc_get_library_code()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_library_code.md),
[`mc_test_library()`](https://datascienceadvice.github.io/multichainr/reference/mc_test_library.md)

## Examples

``` r
if (FALSE) { # \dontrun{
libs <- mc_list_libraries(conn)
} # }
```
