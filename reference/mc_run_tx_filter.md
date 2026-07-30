# Run an existing transaction filter against a transaction

Executes an existing transaction filter on a given transaction, without
performing a blockchain operation.

## Usage

``` r
mc_run_tx_filter(conn, filter, tx)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- filter:

  Character string. Filter name or transaction ID.

- tx:

  Character string. Transaction ID or hex representation.

## Value

The output of the filter (e.g., boolean, transformed transaction).

## See also

[`mc_test_tx_filter`](https://datascienceadvice.github.io/multichainr/reference/mc_test_tx_filter.md),
[`mc_create_tx_filter`](https://datascienceadvice.github.io/multichainr/reference/mc_create_tx_filter.md)

Other filters:
[`mc_approve_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_approve_from.md),
[`mc_create_stream_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream_filter.md),
[`mc_create_tx_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_tx_filter.md),
[`mc_create_upgrade()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_upgrade.md),
[`mc_get_filter_code()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_filter_code.md),
[`mc_list_stream_filters()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_filters.md),
[`mc_list_tx_filters()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_tx_filters.md),
[`mc_list_upgrades()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_upgrades.md),
[`mc_run_stream_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_run_stream_filter.md),
[`mc_test_stream_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_test_stream_filter.md),
[`mc_test_tx_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_test_tx_filter.md)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- mc_run_tx_filter(conn, "myfilter", "txid...")
} # }
```
