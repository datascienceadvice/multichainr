# Issue new asset from specific address

Issues a new asset, but allows specifying the sender address (which must
have sufficient native currency to pay for the transaction). This is
useful when the node has multiple addresses.

## Usage

``` r
mc_issue_from(
  conn,
  from_address,
  to_address,
  name,
  quantity,
  units = 1,
  native_amount = NULL,
  custom_fields = NULL
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- from_address:

  Character string. Address that will pay for and issue the asset.

- to_address:

  Character string. Recipient address (can be the same as
  `from_address`).

- name:

  Either a character string (asset name) or a list of asset parameters
  (e.g., `list(name = "myasset", open = TRUE)`).

- quantity:

  Numeric. Total amount to issue.

- units:

  Numeric. Smallest divisible unit. Default is `1`.

- native_amount:

  Numeric (optional). Amount of native currency to send along with the
  issuance.

- custom_fields:

  List (optional). Custom fields.

## Value

A character string containing the transaction ID.

## See also

[`mc_issue`](https://datascienceadvice.github.io/multichainr/reference/mc_issue.md)
for simpler issuance,
[`mc_issue_more_from`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_more_from.md)
to increase supply.

Other assets:
[`mc_get_asset_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_asset_info.md),
[`mc_get_token_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_token_info.md),
[`mc_issue()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue.md),
[`mc_issue_more()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_more.md),
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
# Issue from a specific address to the same address
txid <- mc_issue_from(conn, from_address = "1A...", to_address = "1A...",
                      name = "myasset", quantity = 100)
} # }
```
