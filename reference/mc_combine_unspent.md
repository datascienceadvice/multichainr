# Combine unspent outputs (UTXOs)

Sends transactions to combine many small unspent transaction outputs
(UTXOs) into a single output. This is used to improve wallet performance
and reduce the size of the wallet's UTXO set.

## Usage

``` r
mc_combine_unspent(
  conn,
  addresses = "*",
  minconf = 1,
  maxcombines = 100,
  mininputs = 2,
  maxinputs = 100,
  maxtime = 15
)
```

## Arguments

- conn:

  A connection object to the MultiChain node.

- addresses:

  A vector of addresses, or `"*"` for all addresses (default `"*"`).

- minconf:

  Integer. Minimum confirmations (default `1`).

- maxcombines:

  Integer. Maximum number of transactions to create (default `100`).

- mininputs:

  Integer. Minimum number of inputs per transaction (default `2`).

- maxinputs:

  Integer. Maximum number of inputs per transaction (default `100`).

- maxtime:

  Integer. Maximum seconds to spend combining (default `15`).

## Value

A character vector of the transaction IDs (txids) created.

## See also

Other wallet:
[`mc_backup_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_backup_wallet.md),
[`mc_change_wallet_passphrase()`](https://datascienceadvice.github.io/multichainr/reference/mc_change_wallet_passphrase.md),
[`mc_dump_privkey()`](https://datascienceadvice.github.io/multichainr/reference/mc_dump_privkey.md),
[`mc_dump_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_dump_wallet.md),
[`mc_encrypt_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_encrypt_wallet.md),
[`mc_get_wallet_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_wallet_info.md),
[`mc_import_privkey()`](https://datascienceadvice.github.io/multichainr/reference/mc_import_privkey.md),
[`mc_import_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_import_wallet.md),
[`mc_list_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_lock_unspent.md),
[`mc_list_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_unspent.md),
[`mc_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_lock_unspent.md),
[`mc_lock_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_lock_wallet.md),
[`mc_unlock_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_unlock_wallet.md)
