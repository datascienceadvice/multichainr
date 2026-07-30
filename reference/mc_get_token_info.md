# Get information about a specific token (MultiChain 2.2.1+)

Retrieves details about a token (non‑fungible) within a parent asset.
This function requires MultiChain version 2.2.1 or later.

## Usage

``` r
mc_get_token_info(conn, asset, token, verbose = FALSE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- asset:

  Character string. Parent asset name, reference, or issuance
  transaction ID.

- token:

  Character string. Token name or index (e.g., `"token0"`).

- verbose:

  Logical. If `TRUE`, returns detailed information, including token
  details. Default is `FALSE`.

## Value

A list with token information, such as name, quantity, and (if
`verbose`) custom fields.

## See also

[`mc_issue_token`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_token.md)
to issue tokens,
[`mc_list_assets`](https://datascienceadvice.github.io/multichainr/reference/mc_list_assets.md)
to list parent assets.

Other assets:
[`mc_get_asset_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_asset_info.md),
[`mc_issue()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue.md),
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
# Get basic info about token "nft1" in asset "art"
token_info <- mc_get_token_info(conn, "art", "nft1")
} # }
```
