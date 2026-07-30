# Get balances for non-fungible tokens (NFTs)

Specifically retrieves balances for tokens (sub-assets or individual
units) associated with a parent asset.

## Usage

``` r
mc_get_token_balances(
  conn,
  addresses = "*",
  assets = "*",
  minconf = 1,
  include_watch_only = FALSE,
  include_locked = FALSE
)
```

## Arguments

- conn:

  A connection object to the MultiChain node.

- addresses:

  A vector of addresses, or `"*"` (default `"*"`).

- assets:

  A vector of parent asset names/refs, or `"*"` (default `"*"`).

- minconf:

  Integer. Minimum confirmations (default `1`).

- include_watch_only:

  Logical. Include watch-only addresses (default `FALSE`).

- include_locked:

  Logical. Include locked outputs (default `FALSE`).

## Value

A data frame containing token-level balance details.

## See also

Other transactions:
[`mc_get_address_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_address_balances.md),
[`mc_get_address_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_address_transaction.md),
[`mc_get_multi_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_multi_balances.md),
[`mc_get_total_balances()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_total_balances.md),
[`mc_get_tx_out_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_tx_out_data.md),
[`mc_get_wallet_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_wallet_transaction.md),
[`mc_list_address_transactions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_address_transactions.md),
[`mc_list_wallet_transactions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_wallet_transactions.md),
[`mc_send()`](https://datascienceadvice.github.io/multichainr/reference/mc_send.md),
[`mc_send_asset()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset.md),
[`mc_send_asset_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset_from.md),
[`mc_send_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_from.md),
[`mc_send_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data.md),
[`mc_send_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data_from.md)
