# Send payment from a specific address

Sends a payment (native currency or assets) from a specific sender
address. This is the counterpart of
[`mc_send`](https://datascienceadvice.github.io/multichainr/reference/mc_send.md)
for multi‑asset transactions with a chosen source address.

## Usage

``` r
mc_send_from(
  conn,
  from_address,
  to_address,
  amounts,
  comment = "",
  comment_to = ""
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- from_address:

  Character string. Sender address.

- to_address:

  Character string. Recipient address.

- amounts:

  Either a numeric value (native) or a named list of assets.

- comment:

  Character string. Optional transaction comment.

- comment_to:

  Character string. Optional comment‑to field.

## Value

A character string containing the transaction ID.

## See also

[`mc_send`](https://datascienceadvice.github.io/multichainr/reference/mc_send.md),
[`mc_send_asset_from`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset_from.md).

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
[`mc_send_asset_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset_from.md),
[`mc_send_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data.md),
[`mc_send_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Send 2 native coins from one address to another
txid <- mc_send_from(conn, "1A...", "1B...", 2)

# Send assets from a specific address
txid <- mc_send_from(conn, "1A...", "1B...", list(myasset = 100, other = 50))
} # }
```
