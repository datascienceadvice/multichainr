# Get a specific transaction involving a subscribed asset

Returns details of a single transaction that affects a subscribed asset.

## Usage

``` r
mc_get_asset_transaction(conn, asset, txid, verbose = FALSE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- asset:

  Character string. Subscribed asset name, reference, or issuance ID.

- txid:

  Character string. Transaction ID.

- verbose:

  Logical. If `TRUE`, includes additional details.

## Value

A list (or data frame) with transaction details, including inputs,
outputs, and asset movements.

## See also

[`mc_list_asset_transactions`](https://datascienceadvice.github.io/multichainr/reference/mc_list_asset_transactions.md)
to list multiple transactions.

Other asset transactions:
[`mc_list_asset_transactions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_asset_transactions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get details of a specific transaction
tx <- mc_get_asset_transaction(conn, "mycoin", "txid...")
} # }
```
