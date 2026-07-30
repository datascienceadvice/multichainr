# Update asset status (open/closed)

Changes the status of an asset (e.g., to open or close further
issuances). The asset must have been created with the ability to be
updated.

## Usage

``` r
mc_update(conn, asset, params)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- asset:

  Character string. Asset name, reference, or issuance ID.

- params:

  A list of parameters to update, typically `list(open = FALSE)` to
  close further issuances.

## Value

A character string containing the transaction ID.

## See also

[`mc_update_from`](https://datascienceadvice.github.io/multichainr/reference/mc_update_from.md)
to update from a specific address.

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
[`mc_list_assets()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_assets.md),
[`mc_update_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_update_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Close the asset "mycoin" to prevent further issuances
txid <- mc_update(conn, "mycoin", list(open = FALSE))
} # }
```
