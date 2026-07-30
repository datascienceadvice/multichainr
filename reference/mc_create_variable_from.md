# Create a variable from a specific address

Creates a global variable, specifying the address that issues the
transaction.

## Usage

``` r
mc_create_variable_from(conn, from_address, name, open = TRUE, value = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- from_address:

  Character string. Address that pays for and creates the variable.

- name:

  Character string. Variable name.

- open:

  Logical. If `TRUE`, anyone with `create` permissions can edit the
  variable.

- value:

  Optional. Initial JSON value.

## Value

A list containing the result of the RPC call (usually transaction ID).

## See also

Other variables:
[`mc_create_variable()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_variable.md),
[`mc_get_variable_history()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_history.md),
[`mc_get_variable_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_info.md),
[`mc_get_variable_value()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_value.md),
[`mc_list_variables()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_variables.md),
[`mc_set_variable_value()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value.md),
[`mc_set_variable_value_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mc_create_variable_from(conn, "1A...", "myvar", open = TRUE, value = 42)
} # }
```
