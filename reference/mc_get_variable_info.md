# Get information about a variable

Returns metadata about a variable, such as creator, open status,
creation time.

## Usage

``` r
mc_get_variable_info(conn, variable, verbose = FALSE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- variable:

  Character string. Variable name or transaction ID.

- verbose:

  Logical. If `TRUE`, includes additional details.

## Value

A list with variable information.

## See also

Other variables:
[`mc_create_variable()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_variable.md),
[`mc_create_variable_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_variable_from.md),
[`mc_get_variable_history()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_history.md),
[`mc_get_variable_value()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_value.md),
[`mc_list_variables()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_variables.md),
[`mc_set_variable_value()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value.md),
[`mc_set_variable_value_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
info <- mc_get_variable_info(conn, "myvar")
print(info$open)
} # }
```
