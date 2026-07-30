# List unspent transaction outputs

Returns a list of all unspent outputs (UTXOs) available in the wallet.

## Usage

``` r
mc_list_unspent(conn, minconf = 1, maxconf = 999999, addresses = NULL)
```

## Arguments

- conn:

  A connection object to the MultiChain node.

- minconf:

  Integer. Minimum confirmations (default `1`).

- maxconf:

  Integer. Maximum confirmations (default `999999`).

- addresses:

  Optional character vector of addresses to filter the results.

## Value

A data frame containing UTXO details, including `txid`, `vout`,
`address`, `amount`, and associated asset/permission data.

## See also

Other wallet:
[`mc_backup_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_backup_wallet.md),
[`mc_change_wallet_passphrase()`](https://datascienceadvice.github.io/multichainr/reference/mc_change_wallet_passphrase.md),
[`mc_combine_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_combine_unspent.md),
[`mc_dump_privkey()`](https://datascienceadvice.github.io/multichainr/reference/mc_dump_privkey.md),
[`mc_dump_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_dump_wallet.md),
[`mc_encrypt_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_encrypt_wallet.md),
[`mc_get_wallet_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_wallet_info.md),
[`mc_import_privkey()`](https://datascienceadvice.github.io/multichainr/reference/mc_import_privkey.md),
[`mc_import_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_import_wallet.md),
[`mc_list_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_lock_unspent.md),
[`mc_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_lock_unspent.md),
[`mc_lock_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_lock_wallet.md),
[`mc_unlock_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_unlock_wallet.md)
