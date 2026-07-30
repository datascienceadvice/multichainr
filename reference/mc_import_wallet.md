# Import keys from a wallet dump file

Import keys from a wallet dump file

## Usage

``` r
mc_import_wallet(conn, filename, rescan = 0)
```

## Arguments

- conn:

  A connection object to the MultiChain node.

- filename:

  Character. Path to the dump file on the node's machine.

- rescan:

  Integer. The block number to start rescanning from (default `0`).

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
[`mc_import_privkey()`](https://datascienceadvice.github.io/multichainr/reference/mc_import_privkey.md),
[`mc_list_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_lock_unspent.md),
[`mc_list_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_unspent.md),
[`mc_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_lock_unspent.md),
[`mc_lock_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_lock_wallet.md),
[`mc_unlock_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_unlock_wallet.md)
