# Manage testing of libraries and updates locally

Tests a library's code or a specific update without permanently creating
it. The behaviour depends on the arguments:

- If only `js_code` is provided, tests that code as a new library.

- If `library` and optionally `updatename` are given, tests the existing
  library's code (or a specific update).

## Usage

``` r
mc_test_library(conn, library = NULL, updatename = NULL, js_code = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- library:

  Optional character string. Library name or transaction ID.

- updatename:

  Optional character string. Update name (if testing an update).

- js_code:

  Optional character string. JavaScript code (if testing a new library).

## Value

The result of the test (e.g., compiled code, validation output).

## See also

[`mc_create_library`](https://datascienceadvice.github.io/multichainr/reference/mc_create_library.md),
[`mc_add_library_update`](https://datascienceadvice.github.io/multichainr/reference/mc_add_library_update.md)

Other libraries:
[`mc_add_library_update()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_library_update.md),
[`mc_add_library_update_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_library_update_from.md),
[`mc_create_library()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_library.md),
[`mc_get_library_code()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_library_code.md),
[`mc_list_libraries()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_libraries.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Test a new library
mc_test_library(conn, js_code = "function add(a, b) { return a + b; }")

# Test an existing library's active code
mc_test_library(conn, library = "math")
} # }
```
