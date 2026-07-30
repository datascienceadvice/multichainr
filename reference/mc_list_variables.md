# List variables created on the blockchain

Returns a list of variables, with optional filtering, verbosity, and
pagination.

## Usage

``` r
mc_list_variables(
  conn,
  variables = "*",
  verbose = FALSE,
  count = NULL,
  start = NULL
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- variables:

  Character vector of variable names/IDs, or `"*"` (default) for all
  variables.

- verbose:

  Logical. If `TRUE`, returns detailed information.

- count:

  Optional integer. Maximum number of variables to return.

- start:

  Optional integer. Offset for pagination.

## Value

A data frame (via `rpc_res_to_df`) with variable information.

## See also

Other variables:
[`mc_create_variable()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_variable.md),
[`mc_create_variable_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_variable_from.md),
[`mc_get_variable_history()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_history.md),
[`mc_get_variable_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_info.md),
[`mc_get_variable_value()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_variable_value.md),
[`mc_set_variable_value()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value.md),
[`mc_set_variable_value_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
all_vars <- mc_list_variables(conn)
first_10 <- mc_list_variables(conn, count = 10)
} # }
```
