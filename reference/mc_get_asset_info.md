# Get information about a specific asset

Retrieves details about an asset on the MultiChain blockchain. The asset
can be identified by its name, reference, or issuance transaction ID.

## Usage

``` r
mc_get_asset_info(conn, asset, verbose = FALSE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- asset:

  Character string. Asset name, reference, or issuance transaction ID.

- verbose:

  Logical. If `TRUE`, returns details about individual issuances (for
  fungible assets with multiple issuances). Default is `FALSE`.

## Value

A list (or data frame, depending on verbosity) containing asset
information such as name, type, total quantity, units, etc.

## See also

[`mc_list_assets`](https://datascienceadvice.github.io/multichainr/reference/mc_list_assets.md)
to list all assets,
[`mc_list_asset_issues`](https://datascienceadvice.github.io/multichainr/reference/mc_list_asset_issues.md)
to list issuances.

Other assets:
[`mc_get_token_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_token_info.md),
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
# Get basic info about an asset named "mycoin"
info <- mc_get_asset_info(conn, "mycoin")
print(info$name)

# Get verbose info with issuance details
info_verbose <- mc_get_asset_info(conn, "mycoin", verbose = TRUE)
} # }
```
