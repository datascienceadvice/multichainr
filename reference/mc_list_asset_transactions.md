# List transactions involving a subscribed asset

Returns a list of recent transactions that affect a subscribed asset.

## Usage

``` r
mc_list_asset_transactions(
  conn,
  asset,
  verbose = FALSE,
  count = 10,
  start = NULL,
  local_ordering = FALSE
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- asset:

  Character string. Subscribed asset name, reference, or issuance ID.

- verbose:

  Logical. If `TRUE`, returns detailed information about each
  transaction.

- count:

  Integer. Number of transactions to return (default 10).

- start:

  Integer (optional). Offset (negative for most recent).

- local_ordering:

  Logical. If `TRUE`, uses local node's transaction ordering (default
  `FALSE`).

## Value

A data frame (via `rpc_res_to_df`) with transaction details.

## See also

[`mc_get_asset_transaction`](https://datascienceadvice.github.io/multichainr/reference/mc_get_asset_transaction.md)
for a single transaction.

Other asset transactions:
[`mc_get_asset_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_asset_transaction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the 10 most recent transactions
txs <- mc_list_asset_transactions(conn, "mycoin")

# Get next 5 transactions with details
more_txs <- mc_list_asset_transactions(conn, "mycoin", verbose = TRUE,
                                       count = 5, start = 10)
} # }
```
