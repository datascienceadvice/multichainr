# Add an update to an existing library

Adds a new version (update) to an existing library. The update mechanism
depends on the library's `updatemode`.

## Usage

``` r
mc_add_library_update(conn, library, updatename, js_code)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- library:

  Character string. Library name or transaction ID.

- updatename:

  Character string. Name of this update (must be unique).

- js_code:

  Character string. The new JavaScript code (or patch).

## Value

A list containing the result of the RPC call (usually transaction ID).

## See also

[`mc_create_library`](https://datascienceadvice.github.io/multichainr/reference/mc_create_library.md),
[`mc_add_library_update_from`](https://datascienceadvice.github.io/multichainr/reference/mc_add_library_update_from.md)

Other libraries:
[`mc_add_library_update_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_library_update_from.md),
[`mc_create_library()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_library.md),
[`mc_get_library_code()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_library_code.md),
[`mc_list_libraries()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_libraries.md),
[`mc_test_library()`](https://datascienceadvice.github.io/multichainr/reference/mc_test_library.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mc_add_library_update(conn, "math", "v2", "function add(a, b) { return a + b + 1; }")
} # }
```
