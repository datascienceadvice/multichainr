# Import one or more private keys

Adds private keys to the node's wallet.

## Usage

``` r
mc_import_privkey(conn, privkeys, label = "", rescan = TRUE)
```

## Arguments

- conn:

  A connection object to the MultiChain node.

- privkeys:

  A character string or vector of private keys (WIF format).

- label:

  Optional character string. A label to assign to the addresses.

- rescan:

  Logical or Integer. If `TRUE`, rescans the entire blockchain. Can also
  be an integer representing the block height to start scanning from.

## Value

Returns `NULL` on success.

## See also

Other wallet:
[`mc_backup_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_backup_wallet.md),
[`mc_change_wallet_passphrase()`](https://datascienceadvice.github.io/multichainr/reference/mc_change_wallet_passphrase.md),
[`mc_combine_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_combine_unspent.md),
[`mc_dump_privkey()`](https://datascienceadvice.github.io/multichainr/reference/mc_dump_privkey.md),
[`mc_dump_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_dump_wallet.md),
[`mc_encrypt_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_encrypt_wallet.md),
[`mc_get_wallet_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_wallet_info.md),
[`mc_import_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_import_wallet.md),
[`mc_list_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_lock_unspent.md),
[`mc_list_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_unspent.md),
[`mc_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_lock_unspent.md),
[`mc_lock_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_lock_wallet.md),
[`mc_unlock_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_unlock_wallet.md)
