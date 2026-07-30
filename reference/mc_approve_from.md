# Approve or disapprove an upgrade or filter

Sends an approval or disapproval transaction from a specific address
(must have admin permissions). This is used to vote on upgrades or to
approve/disapprove filters.

## Usage

``` r
mc_approve_from(conn, from_address, entity, approve)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- from_address:

  Character string. Admin address that issues the approval.

- entity:

  Character string. Name or transaction ID of the upgrade/filter.

- approve:

  Either a logical (for global upgrades/filters) or a list of the form
  `list("for" = "stream", approve = TRUE)` for stream‑specific filter
  approval.

## Value

A list containing the result of the RPC call (usually transaction ID).

## See also

[`mc_create_upgrade`](https://datascienceadvice.github.io/multichainr/reference/mc_create_upgrade.md),
[`mc_create_stream_filter`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream_filter.md)

Other filters:
[`mc_create_stream_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream_filter.md),
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
# Approve a global upgrade
mc_approve_from(conn, "admin_address", "speedup", approve = TRUE)

# Approve a stream filter for a specific stream
mc_approve_from(conn, "admin_address", "myfilter",
                approve = list("for" = "mystream", approve = TRUE))
} # }
```
