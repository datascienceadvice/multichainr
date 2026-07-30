# Send a single asset to an address

Convenience function to send a single asset (or native currency) to an
address. Equivalent to
[`mc_send`](https://datascienceadvice.github.io/multichainr/reference/mc_send.md)
but with a simpler interface.

## Usage

``` r
mc_send_asset(
  conn,
  address,
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

- address:

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

[`mc_send_asset_from`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset_from.md)
to specify sender,
[`mc_send`](https://datascienceadvice.github.io/multichainr/reference/mc_send.md)
for multiple assets.

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
[`mc_send_asset_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset_from.md),
[`mc_send_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_from.md),
[`mc_send_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data.md),
[`mc_send_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Send 100 units of "myasset"
txid <- mc_send_asset(conn, "1A...", "myasset", 100)

# Send asset along with 0.5 native coins
txid <- mc_send_asset(conn, "1A...", "myasset", 100, native_amount = 0.5)
} # }
```
