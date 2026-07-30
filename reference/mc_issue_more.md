# Issue more of an existing fungible asset

Increases the supply of a previously issued fungible asset. The asset
must be open for further issuances (i.e., its `open` property must be
`TRUE`).

## Usage

``` r
mc_issue_more(
  conn,
  address,
  asset,
  quantity,
  native_amount = NULL,
  custom_fields = NULL
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- address:

  Character string. Address that will receive the newly issued units.

- asset:

  Character string. Asset name, reference, or issuance transaction ID.

- quantity:

  Numeric. Additional quantity to issue.

- native_amount:

  Numeric (optional). Amount of native currency to send.

- custom_fields:

  List (optional). Custom fields (overwrites existing ones if present).

## Value

A character string containing the transaction ID.

## See also

[`mc_issue`](https://datascienceadvice.github.io/multichainr/reference/mc_issue.md)
for initial issuance,
[`mc_issue_more_from`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_more_from.md)
to issue from a specific address.

Other assets:
[`mc_get_asset_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_asset_info.md),
[`mc_get_token_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_token_info.md),
[`mc_issue()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue.md),
[`mc_issue_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_from.md),
[`mc_issue_more_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_more_from.md),
[`mc_issue_token()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_token.md),
[`mc_issue_token_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_token_from.md),
[`mc_list_asset_issues()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_asset_issues.md),
[`mc_list_assets()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_assets.md),
[`mc_update()`](https://datascienceadvice.github.io/multichainr/reference/mc_update.md),
[`mc_update_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_update_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Increase supply of "mycoin" by 500 units
txid <- mc_issue_more(conn, "1A...", "mycoin", quantity = 500)
} # }
```
