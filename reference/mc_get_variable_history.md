# List historical values of a variable

Retrieves the update history of a variable, showing previous values and
their transaction IDs.

## Usage

``` r
mc_get_variable_history(
  conn,
  variable,
  verbose = FALSE,
  count = 10,
  start = NULL
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- variable:

  Character string. Variable name or transaction ID.

- verbose:

  Logical. If `TRUE`, returns detailed entries.

- count:

  Integer. Number of historical entries to return (default 10).

- start:

  Optional integer. Offset (positive for forward, negative for
  backward). If omitted, the most recent entries are returned.

## Value

A data frame (via `rpc_res_to_df`) with history entries.

## See also

Other variables:
[`mc_create_variable()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_variable.md),
[`mc_create_variable_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_variable_from.md),
[`mc_get_variable_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_info.md),
[`mc_get_variable_value()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_value.md),
[`mc_list_variables()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_variables.md),
[`mc_set_variable_value()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value.md),
[`mc_set_variable_value_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
history <- mc_get_variable_history(conn, "myvar", count = 5)
} # }
```
