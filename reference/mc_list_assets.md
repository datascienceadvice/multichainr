# List blockchain assets

Returns a list of assets on the blockchain, with optional filtering and
pagination.

## Usage

``` r
mc_list_assets(conn, assets = "*", verbose = FALSE, count = NULL, start = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- assets:

  Asset filter. Can be a single asset name/ref/txid, a vector of such
  identifiers, or `"*"` (default) to list all assets.

- verbose:

  Logical. If `TRUE`, returns detailed information (e.g., issuances,
  open status). Default is `FALSE`.

- count:

  Integer (optional). Maximum number of assets to return.

- start:

  Integer (optional). Offset for pagination.

## Value

A data frame (via `rpc_res_to_df`) with asset information. If
`verbose = FALSE`, columns include `name`, `ref`, `issuetxid`, etc. If
`verbose = TRUE`, additional details like issuances and open status are
included.

## See also

[`mc_get_asset_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_asset_info.md)
for single asset details.

Other assets:
[`mc_get_asset_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_asset_info.md),
[`mc_get_token_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_token_info.md),
[`mc_issue()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue.md),
[`mc_issue_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_from.md),
[`mc_issue_more()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_more.md),
[`mc_issue_more_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_more_from.md),
[`mc_issue_token()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_token.md),
[`mc_issue_token_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_token_from.md),
[`mc_list_asset_issues()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_asset_issues.md),
[`mc_update()`](https://datascienceadvice.github.io/multichainr/reference/mc_update.md),
[`mc_update_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_update_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# List all assets
all_assets <- mc_list_assets(conn)

# Get details for a specific asset
asset_detail <- mc_list_assets(conn, assets = "mycoin", verbose = TRUE)

# List first 10 assets
first_10 <- mc_list_assets(conn, count = 10)
} # }
```
