# Send payment or assets to an address

Sends a payment (native currency or assets) to a specified address. This
is a general‑purpose sending function that can handle multiple asset
types in a single transaction.

## Usage

``` r
mc_send(conn, address, amounts, comment = "", comment_to = "")
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- address:

  Character string. Recipient address.

- amounts:

  Either a numeric value (for native currency) or a named list
  specifying assets and quantities, e.g.,
  `list(asset1 = 10, asset2 = 5)`. For metadata, see
  [`mc_send_with_data`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data.md).

- comment:

  Character string. Optional transaction comment (stored on chain).

- comment_to:

  Character string. Optional comment‑to field.

## Value

A character string containing the transaction ID (txid).

## See also

[`mc_send_from`](https://datascienceadvice.github.io/multichainr/reference/mc_send_from.md)
to specify the sender,
[`mc_send_asset`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset.md)
for single‑asset convenience.

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
[`mc_send_asset()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset.md),
[`mc_send_asset_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset_from.md),
[`mc_send_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_from.md),
[`mc_send_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data.md),
[`mc_send_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Send 1.5 native coins
txid <- mc_send(conn, "1A...", 1.5)

# Send assets only
txid <- mc_send(conn, "1A...", list(myasset = 100))

# Send both native and assets
txid <- mc_send(conn, "1A...", list(0.5, myasset = 50))
} # }
```
