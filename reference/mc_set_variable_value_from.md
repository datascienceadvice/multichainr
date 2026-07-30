# Set variable value from specific address

Updates a variable's value, specifying the address that pays for the
transaction.

## Usage

``` r
mc_set_variable_value_from(conn, from_address, variable, value = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- from_address:

  Character string. Address that pays for the update.

- variable:

  Character string. Variable name or transaction ID.

- value:

  Optional. New value (any JSON structure). If `NULL`, the variable is
  deleted.

## Value

A list containing the result of the RPC call (usually transaction ID).

## See also

Other variables:
[`mc_create_variable()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_variable.md),
[`mc_create_variable_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_variable_from.md),
[`mc_get_variable_history()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_history.md),
[`mc_get_variable_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_info.md),
[`mc_get_variable_value()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_value.md),
[`mc_list_variables()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_variables.md),
[`mc_set_variable_value()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mc_set_variable_value_from(conn, "1A...", "myvar", value = 100)
} # }
```
