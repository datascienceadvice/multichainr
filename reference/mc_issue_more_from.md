# Issue more of an asset from specific address

Increases the supply of a fungible asset, specifying the address that
pays for and initiates the transaction.

## Usage

``` r
mc_issue_more_from(
  conn,
  from_address,
  to_address,
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

- from_address:

  Character string. Address that will pay for and issue the additional
  units.

- to_address:

  Character string. Recipient address.

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

[`mc_issue_more`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_more.md)
for simpler usage,
[`mc_issue_from`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_from.md)
for initial issuance.

Other assets:
[`mc_get_asset_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_asset_info.md),
[`mc_get_token_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_token_info.md),
[`mc_issue()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue.md),
[`mc_issue_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_from.md),
[`mc_issue_more()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_more.md),
[`mc_issue_token()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_token.md),
[`mc_issue_token_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_token_from.md),
[`mc_list_asset_issues()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_asset_issues.md),
[`mc_list_assets()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_assets.md),
[`mc_update()`](https://datascienceadvice.github.io/multichainr/reference/mc_update.md),
[`mc_update_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_update_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Issue more from a specific address to another address
txid <- mc_issue_more_from(conn, "1A...", "1B...", "mycoin", quantity = 100)
} # }
```
