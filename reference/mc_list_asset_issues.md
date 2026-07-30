# List issuance events for an asset

Returns a list of all issuance transactions (initial and subsequent) for
a given asset.

## Usage

``` r
mc_list_asset_issues(conn, asset, verbose = FALSE, count = NULL, start = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- asset:

  Character string. Asset name, reference, or issuance ID.

- verbose:

  Logical. If `TRUE`, includes details about issuers and custom fields.
  Default is `FALSE`.

- count:

  Integer (optional). Maximum number of issuances to return.

- start:

  Integer (optional). Offset (positive for forward, negative for
  backward from the most recent). Use a negative value to get the most
  recent issuances first.

## Value

A data frame (converted via `rpc_res_to_df`) with one row per issuance.
Columns typically include `txid`, `issuer`, `quantity`, `units`, etc.

## See also

[`mc_list_assets`](https://datascienceadvice.github.io/multichainr/reference/mc_list_assets.md)
to list assets,
[`mc_get_asset_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_asset_info.md)
for asset summary.

Other assets:
[`mc_get_asset_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_asset_info.md),
[`mc_get_token_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_token_info.md),
[`mc_issue()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue.md),
[`mc_issue_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_from.md),
[`mc_issue_more()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_more.md),
[`mc_issue_more_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_more_from.md),
[`mc_issue_token()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_token.md),
[`mc_issue_token_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_token_from.md),
[`mc_list_assets()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_assets.md),
[`mc_update()`](https://datascienceadvice.github.io/multichainr/reference/mc_update.md),
[`mc_update_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_update_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all issuances of "mycoin"
issues <- mc_list_asset_issues(conn, "mycoin")

# Get the most recent 5 issuances with details
recent <- mc_list_asset_issues(conn, "mycoin", verbose = TRUE,
                               count = 5, start = -5)
} # }
```
