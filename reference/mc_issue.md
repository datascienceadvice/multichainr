# Issue new asset

Creates a new asset on the MultiChain blockchain. The asset can be
fungible or non‑fungible, and its properties (e.g., open/restricted,
divisible) are specified via the `name` parameter.

## Usage

``` r
mc_issue(
  conn,
  address,
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

- address:

  Character string. Wallet address that will receive the issued assets.

- name:

  Either a character string (asset name) or a list of asset parameters
  (e.g., `list(name = "myasset", open = TRUE)`). See MultiChain
  documentation for supported parameters.

- quantity:

  Numeric. Total amount to issue. For non‑fungible assets, this is
  typically `1`.

- units:

  Numeric. The smallest divisible unit (e.g., `0.01` means the asset is
  divisible to two decimal places). Default is `1` (indivisible).

- native_amount:

  Numeric (optional). Amount of native currency (coins) to send together
  with the asset issuance.

- custom_fields:

  List (optional). Custom fields to attach to the asset.

## Value

A character string containing the transaction ID of the issuance.

## See also

[`mc_issue_from`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_from.md)
to issue from a specific address,
[`mc_issue_more`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_more.md)
to increase supply of a fungible asset.

Other assets:
[`mc_get_asset_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_asset_info.md),
[`mc_get_token_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_token_info.md),
[`mc_issue_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_from.md),
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
# Issue a simple fungible asset
txid <- mc_issue(conn, "1A...", "mycoin", quantity = 1000, units = 0.01)

# Issue a restricted, non‑fungible asset with custom fields
params <- list(name = "artwork", open = FALSE, restrict = TRUE)
txid <- mc_issue(conn, "1A...", params, quantity = 1, units = 1,
                 custom_fields = list(author = "Picasso"))
} # }
```
