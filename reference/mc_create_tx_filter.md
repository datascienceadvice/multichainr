# Create a transaction filter

Creates a new transaction filter on the blockchain. Transaction filters
are JavaScript programs that validate or transform transactions.

## Usage

``` r
mc_create_tx_filter(conn, name, options, js_code)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- name:

  Character string. Name of the filter (must be unique).

- options:

  List of filter options. Usually includes `for` (target asset or
  stream) and `libraries` (list of library names). Example:
  `list("for" = "asset1", libraries = list("lib1"))`.

- js_code:

  Character string. The JavaScript code for the filter.

## Value

A list containing the result of the RPC call (usually transaction ID).

## See also

[`mc_create_stream_filter`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream_filter.md),
[`mc_list_tx_filters`](https://datascienceadvice.github.io/multichainr/reference/mc_list_tx_filters.md)

Other filters:
[`mc_approve_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_approve_from.md),
[`mc_create_stream_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream_filter.md),
[`mc_create_upgrade()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_upgrade.md),
[`mc_get_filter_code()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_filter_code.md),
[`mc_list_stream_filters()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_filters.md),
[`mc_list_tx_filters()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_tx_filters.md),
[`mc_list_upgrades()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_upgrades.md),
[`mc_run_stream_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_run_stream_filter.md),
[`mc_run_tx_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_run_tx_filter.md),
[`mc_test_stream_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_test_stream_filter.md),
[`mc_test_tx_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_test_tx_filter.md)

## Examples

``` r
if (FALSE) { # \dontrun{
js_code <- "function filter(tx) { return tx.vin.length > 0; }"
mc_create_tx_filter(conn, "myfilter", list("for" = "asset1"), js_code)
} # }
```
