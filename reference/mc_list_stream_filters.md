# List stream filters

Returns a list of stream filters on the blockchain, with optional
filtering and verbosity.

## Usage

``` r
mc_list_stream_filters(conn, filters = "*", verbose = FALSE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- filters:

  Character vector of filter names/IDs, or `"*"` (default) for all
  filters.

- verbose:

  Logical. If `TRUE`, returns detailed information.

## Value

A data frame (via `rpc_res_to_df`) with filter information.

## See also

[`mc_create_stream_filter`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream_filter.md),
[`mc_list_tx_filters`](https://datascienceadvice.github.io/multichainr/reference/mc_list_tx_filters.md)

Other filters:
[`mc_approve_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_approve_from.md),
[`mc_create_stream_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream_filter.md),
[`mc_create_tx_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_tx_filter.md),
[`mc_create_upgrade()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_upgrade.md),
[`mc_get_filter_code()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_filter_code.md),
[`mc_list_tx_filters()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_tx_filters.md),
[`mc_list_upgrades()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_upgrades.md),
[`mc_run_stream_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_run_stream_filter.md),
[`mc_run_tx_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_run_tx_filter.md),
[`mc_test_stream_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_test_stream_filter.md),
[`mc_test_tx_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_test_tx_filter.md)

## Examples

``` r
if (FALSE) { # \dontrun{
all_filters <- mc_list_stream_filters(conn)
} # }
```
