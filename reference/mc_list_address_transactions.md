# List transactions for a specific address

Returns a list of the most recent transactions involving the specified
address.

## Usage

``` r
mc_list_address_transactions(
  conn,
  address,
  count = 10,
  skip = 0,
  verbose = FALSE
)
```

## Arguments

- conn:

  A connection object to the MultiChain node.

- address:

  Character. The MultiChain address to query.

- count:

  Integer. The number of transactions to return (default `10`).

- skip:

  Integer. The number of transactions to skip (default `0`).

- verbose:

  Logical. If `TRUE`, provides details of inputs and outputs (default
  `FALSE`).

## Value

A data frame of transaction history for the address.

## See also

Other transactions:
[`mc_get_address_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_address_balances.md),
[`mc_get_address_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_address_transaction.md),
[`mc_get_multi_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_multi_balances.md),
[`mc_get_token_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_token_balances.md),
[`mc_get_total_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_total_balances.md),
[`mc_get_tx_out_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_tx_out_data.md),
[`mc_get_wallet_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_wallet_transaction.md),
[`mc_list_wallet_transactions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_wallet_transactions.md),
[`mc_send()`](https://datascienceadvice.github.io/multichainr/reference/mc_send.md),
[`mc_send_asset()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset.md),
[`mc_send_asset_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset_from.md),
[`mc_send_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_from.md),
[`mc_send_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data.md),
[`mc_send_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data_from.md)
