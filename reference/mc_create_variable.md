# Create a new variable

Creates a global variable on the blockchain. Variables are key‑value
stores that can be read by filters and transactions.

## Usage

``` r
mc_create_variable(conn, name, open = TRUE, value = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- name:

  Character string. Variable name (must be unique).

- open:

  Logical. If `TRUE`, anyone with `create` permissions can edit the
  variable. If `FALSE`, only admins can edit.

- value:

  Optional. Initial JSON value (list, number, string, etc.).

## Value

A list containing the result of the RPC call (usually transaction ID).

## See also

[`mc_set_variable_value`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value.md),
[`mc_get_variable_value`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_value.md)

Other variables:
[`mc_create_variable_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_variable_from.md),
[`mc_get_variable_history()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_history.md),
[`mc_get_variable_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_info.md),
[`mc_get_variable_value()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_value.md),
[`mc_list_variables()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_variables.md),
[`mc_set_variable_value()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value.md),
[`mc_set_variable_value_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mc_create_variable(conn, "myvar", open = TRUE, value = list(key = "value"))
} # }
```
