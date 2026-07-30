# Create a stream filter

Creates a new stream filter on the blockchain. Stream filters are
JavaScript programs that can be attached to streams to validate or
transform items.

## Usage

``` r
mc_create_stream_filter(conn, name, options, js_code)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- name:

  Character string. Name of the filter (must be unique).

- options:

  List of filter options. Typically includes `libraries` (list of
  library names) that the filter depends on. Example:
  `list(libraries = list("mylib"))`.

- js_code:

  Character string. The JavaScript code for the filter.

## Value

A list containing the result of the RPC call (usually transaction ID).

## See also

[`mc_create_tx_filter`](https://datascienceadvice.github.io/multichainr/reference/mc_create_tx_filter.md),
[`mc_list_stream_filters`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_filters.md)

Other filters:
[`mc_approve_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_approve_from.md),
[`mc_create_tx_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_tx_filter.md),
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
js_code <- "function filter(stream, item) { return true; }"
mc_create_stream_filter(conn, "myfilter", list(libraries = list()), js_code)
} # }
```
