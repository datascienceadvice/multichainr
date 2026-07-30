# Create a new library

Creates a JavaScript library on the blockchain. Libraries contain
reusable code that can be imported by filters.

## Usage

``` r
mc_create_library(
  conn,
  name,
  updatemode = c("none", "instant", "approve"),
  js_code
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- name:

  Character string. Library name (must be unique).

- updatemode:

  Character string. How library updates are handled: `"none"` – no
  updates allowed; `"instant"` – updates take effect immediately;
  `"approve"` – updates require admin approval.

- js_code:

  Character string. The JavaScript code for the library.

## Value

A list containing the result of the RPC call (usually transaction ID).

## See also

[`mc_add_library_update`](https://datascienceadvice.github.io/multichainr/reference/mc_add_library_update.md),
[`mc_list_libraries`](https://datascienceadvice.github.io/multichainr/reference/mc_list_libraries.md)

Other libraries:
[`mc_add_library_update()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_library_update.md),
[`mc_add_library_update_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_library_update_from.md),
[`mc_get_library_code()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_library_code.md),
[`mc_list_libraries()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_libraries.md),
[`mc_test_library()`](https://datascienceadvice.github.io/multichainr/reference/mc_test_library.md)

## Examples

``` r
if (FALSE) { # \dontrun{
js_code <- "function add(a, b) { return a + b; }"
mc_create_library(conn, "math", updatemode = "none", js_code)
} # }
```
