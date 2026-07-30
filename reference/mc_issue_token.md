# Issue tokens for a non-fungible asset (NFT)

Creates one or more tokens (non‑fungible items) under a parent asset.
The parent asset must be non‑fungible (e.g., issued with
`type = "nonfungible"`).

## Usage

``` r
mc_issue_token(
  conn,
  address,
  asset,
  token,
  quantity,
  native_amount = NULL,
  token_details = NULL
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- address:

  Character string. Address that will receive the tokens.

- asset:

  Character string. Parent asset name, reference, or issuance ID.

- token:

  Character string. Token name (must be unique within the asset).

- quantity:

  Numeric. Number of token units to issue (usually `1`).

- native_amount:

  Numeric (optional). Amount of native currency to send.

- token_details:

  List (optional). Custom details for the token (e.g.,
  `list(description = "My NFT")`).

## Value

A character string containing the transaction ID.

## See also

[`mc_get_token_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_token_info.md)
to query token details,
[`mc_issue_token_from`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_token_from.md)
for using a specific sender address.

Other assets:
[`mc_get_asset_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_asset_info.md),
[`mc_get_token_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_token_info.md),
[`mc_issue()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue.md),
[`mc_issue_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_from.md),
[`mc_issue_more()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_more.md),
[`mc_issue_more_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_more_from.md),
[`mc_issue_token_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_token_from.md),
[`mc_list_asset_issues()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_asset_issues.md),
[`mc_list_assets()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_assets.md),
[`mc_update()`](https://datascienceadvice.github.io/multichainr/reference/mc_update.md),
[`mc_update_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_update_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Issue a token "painting1" under asset "art"
txid <- mc_issue_token(conn, "1A...", "art", "painting1", quantity = 1,
                       token_details = list(artist = "Monet", year = 2025))
} # }
```
