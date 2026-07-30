# Update asset status from specific address

Changes the status of an asset (e.g., open/close) and specifies the
address that pays for and authorizes the update.

## Usage

``` r
mc_update_from(conn, from_address, asset, params)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- from_address:

  Character string. Address that pays for the transaction.

- asset:

  Character string. Asset name, reference, or issuance ID.

- params:

  A list of parameters to update.

## Value

A character string containing the transaction ID.

## See also

[`mc_update`](https://datascienceadvice.github.io/multichainr/reference/mc_update.md)
for simpler usage.

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
[`mc_update()`](https://datascienceadvice.github.io/multichainr/reference/mc_update.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Close the asset from a specific address
txid <- mc_update_from(conn, "1A...", "mycoin", list(open = FALSE))
} # }
```
