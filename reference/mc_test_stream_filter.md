# Test a stream filter before creation

Tests a stream filter's JavaScript code against a specific stream item
(transaction and optional vout) without permanently creating the filter.

## Usage

``` r
mc_test_stream_filter(conn, options, js_code, tx = NULL, vout = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- options:

  List of filter options (similar to `mc_create_stream_filter`).

- js_code:

  Character string. JavaScript code to test.

- tx:

  Optional character string. Transaction ID or hex representation of the
  stream item's transaction.

- vout:

  Optional integer. Output index if the stream item is in a transaction
  output.

## Value

The result of the filter evaluation.

## See also

[`mc_create_stream_filter`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream_filter.md),
[`mc_run_stream_filter`](https://datascienceadvice.github.io/multichainr/reference/mc_run_stream_filter.md)

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
[`mc_run_tx_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_run_tx_filter.md),
[`mc_test_tx_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_test_tx_filter.md)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- mc_test_stream_filter(conn, list(libraries = list()),
                                "function filter(stream, item) { return true; }",
                                tx = "txid...", vout = 0)
} # }
```
