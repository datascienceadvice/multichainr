# Send payment from specific address with metadata

Sends a transaction with inline metadata from a specified sender
address. Combines the capabilities of
[`mc_send_from`](https://datascienceadvice.github.io/multichainr/reference/mc_send_from.md)
and
[`mc_send_with_data`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data.md).

## Usage

``` r
mc_send_with_data_from(conn, from_address, to_address, amounts, data)
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

- data:

  Data to embed. Can be a string, list (converted to JSON), etc.

## Value

A character string containing the transaction ID.

## See also

[`mc_send_with_data`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data.md),
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
[`mc_send_asset_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset_from.md),
[`mc_send_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_from.md),
[`mc_send_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Send from a specific address with metadata
txid <- mc_send_with_data_from(conn, "1A...", "1B...", 0.5,
                               list(reference = "invoice123"))
} # }
```
