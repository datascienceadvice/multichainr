# Get JavaScript code for a library

Retrieves the JavaScript code of a library. By default, returns the
active code. If `updatename` is provided, returns the code of that
specific update (or the initial code if `updatename = ""`).

## Usage

``` r
mc_get_library_code(conn, library, updatename = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- library:

  Character string. Library name or transaction ID.

- updatename:

  Optional character string. Update name. If omitted, returns the active
  code. Use `""` to retrieve the initial code.

## Value

A list containing the library code and metadata.

## See also

[`mc_get_filter_code`](https://datascienceadvice.github.io/multichainr/reference/mc_get_filter_code.md)
for filters.

Other libraries:
[`mc_add_library_update()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_library_update.md),
[`mc_add_library_update_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_library_update_from.md),
[`mc_create_library()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_library.md),
[`mc_list_libraries()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_libraries.md),
[`mc_test_library()`](https://datascienceadvice.github.io/multichainr/reference/mc_test_library.md)

## Examples

``` r
if (FALSE) { # \dontrun{
active_code <- mc_get_library_code(conn, "math")
initial_code <- mc_get_library_code(conn, "math", updatename = "")
} # }
```
