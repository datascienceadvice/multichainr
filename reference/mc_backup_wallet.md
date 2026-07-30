# Backup the wallet file

Safely copies the `wallet.dat` file to a specified destination.

## Usage

``` r
mc_backup_wallet(conn, filename)
```

## Arguments

- conn:

  A connection object to the MultiChain node.

- filename:

  Character. The full path and filename for the backup. **Note:** This
  path is relative to the machine where the MultiChain node is running.

## Value

Invisibly returns the RPC result (typically `NULL`) on success.

## See also

Other wallet:
[`mc_change_wallet_passphrase()`](https://datascienceadvice.github.io/multichainr/reference/mc_change_wallet_passphrase.md),
[`mc_combine_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_combine_unspent.md),
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
