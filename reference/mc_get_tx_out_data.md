# Retrieve full hex data from a transaction output

This function is used to retrieve large data payloads (like stream data)
that may have been truncated in standard transaction calls.

## Usage

``` r
mc_get_tx_out_data(conn, txid, vout, count_bytes = NULL, start_byte = 0)
```

## Arguments

- conn:

  A connection object to the MultiChain node.

- txid:

  Character. The transaction ID containing the output.

- vout:

  Integer. The index of the output (starting from 0).

- count_bytes:

  Integer. The number of bytes to retrieve. If `NULL`, returns all data.

- start_byte:

  Integer. The byte offset to start reading from (default `0`).

## Value

A list or string containing the hex data from the transaction output.

## See also

Other transactions:
[`mc_get_address_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_address_balances.md),
[`mc_get_address_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_address_transaction.md),
[`mc_get_multi_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_multi_balances.md),
[`mc_get_token_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_token_balances.md),
[`mc_get_total_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_total_balances.md),
[`mc_get_wallet_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_wallet_transaction.md),
[`mc_list_address_transactions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_address_transactions.md),
[`mc_list_wallet_transactions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_wallet_transactions.md),
[`mc_send()`](https://datascienceadvice.github.io/multichainr/reference/mc_send.md),
[`mc_send_asset()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset.md),
[`mc_send_asset_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset_from.md),
[`mc_send_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_from.md),
[`mc_send_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data.md),
[`mc_send_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data_from.md)
