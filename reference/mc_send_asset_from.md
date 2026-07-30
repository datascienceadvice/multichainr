# Send a single asset from a specific address

Sends an asset (or native currency) from a specific sender address.
Useful when the node has multiple addresses and you want to control
which address the funds are taken from.

## Usage

``` r
mc_send_asset_from(
  conn,
  from_address,
  to_address,
  asset,
  quantity,
  native_amount = 0,
  comment = "",
  comment_to = ""
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- from_address:

  Character string. Sender address (must belong to the node's wallet).

- to_address:

  Character string. Recipient address.

- asset:

  Character string. Asset name, reference, or issuance transaction ID.

- quantity:

  Numeric. Amount of the asset to send.

- native_amount:

  Numeric. Amount of native currency to send (default 0).

- comment:

  Character string. Optional transaction comment.

- comment_to:

  Character string. Optional comment‑to field.

## Value

A character string containing the transaction ID.

## See also

[`mc_send_asset`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset.md),
[`mc_send_from`](https://datascienceadvice.github.io/multichainr/reference/mc_send_from.md).

Other transactions:
[`mc_get_address_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_address_balances.md),
[`mc_get_address_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_address_transaction.md),
[`mc_get_multi_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_multi_balances.md),
[`mc_get_token_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_token_balances.md),
[`mc_get_total_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_total_balances.md),
[`mc_get_tx_out_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_tx_out_data.md),
[`mc_get_wallet_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_wallet_transaction.md),
[`mc_list_address_transactions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_address_transactions.md),
[`mc_list_wallet_transactions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_wallet_transactions.md),
[`mc_send()`](https://datascienceadvice.github.io/multichainr/reference/mc_send.md),
[`mc_send_asset()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset.md),
[`mc_send_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_from.md),
[`mc_send_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data.md),
[`mc_send_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Send from a specific address
txid <- mc_send_asset_from(conn, "1A...", "1B...", "myasset", 50)
} # }
```
