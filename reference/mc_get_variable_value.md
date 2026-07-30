# Retrieve the latest value of a variable

Returns the current value of a variable.

## Usage

``` r
mc_get_variable_value(conn, variable)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- variable:

  Character string. Variable name or transaction ID.

## Value

The current value (any JSON type).

## See also

[`mc_get_variable_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_info.md),
[`mc_get_variable_history`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_history.md)

Other variables:
[`mc_create_variable()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_variable.md),
[`mc_create_variable_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_variable_from.md),
[`mc_get_variable_history()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_history.md),
[`mc_get_variable_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_info.md),
[`mc_list_variables()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_variables.md),
[`mc_set_variable_value()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value.md),
[`mc_set_variable_value_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
val <- mc_get_variable_value(conn, "myvar")
} # }
```
