# Add an update to a library from a specific address

Similar to
[`mc_add_library_update`](https://datascienceadvice.github.io/multichainr/reference/mc_add_library_update.md),
but allows specifying the sending address.

## Usage

``` r
mc_add_library_update_from(conn, from_address, library, updatename, js_code)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- from_address:

  Character string. Address that pays for and issues the update.

- library:

  Character string. Library name or transaction ID.

- updatename:

  Character string. Name of this update.

- js_code:

  Character string. The new JavaScript code (or patch).

## Value

A list containing the result of the RPC call (usually transaction ID).

## See also

Other libraries:
[`mc_add_library_update()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_library_update.md),
[`mc_create_library()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_library.md),
[`mc_get_library_code()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_library_code.md),
[`mc_list_libraries()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_libraries.md),
[`mc_test_library()`](https://datascienceadvice.github.io/multichainr/reference/mc_test_library.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mc_add_library_update_from(conn, "1A...", "math", "v2", "function add(a, b) { return a + b; }")
} # }
```
