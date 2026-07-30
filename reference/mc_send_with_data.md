# Send payment with inline metadata

Sends a transaction that includes arbitrary data (metadata) attached to
the output. The data can be text, JSON, or any binary data
(hex‑encoded). This is useful for storing small amounts of information
on the blockchain.

## Usage

``` r
mc_send_with_data(conn, address, amounts, data)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- address:

  Character string. Recipient address.

- amounts:

  Either a numeric value (native) or a named list of assets.

- data:

  Data to embed. Can be a character string (will be hex‑encoded), a list
  (will be converted to JSON then hex), or raw binary (not directly).
  The function automatically converts lists to JSON and then to hex.

## Value

A character string containing the transaction ID.

## See also

[`mc_send_with_data_from`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data_from.md)
to specify sender,
[`mc_send`](https://datascienceadvice.github.io/multichainr/reference/mc_send.md)
for simple payments.

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
[`mc_send_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Send with a text note
txid <- mc_send_with_data(conn, "1A...", 0.1, "Hello, blockchain!")

# Send with structured JSON metadata
metadata <- list(id = 123, action = "transfer", tag = "payment")
txid <- mc_send_with_data(conn, "1A...", list(myasset = 10), metadata)
} # }
```
