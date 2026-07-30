# Encrypt the wallet

Encrypts the wallet with a passphrase for the first time.

## Usage

``` r
mc_encrypt_wallet(conn, passphrase)
```

## Arguments

- conn:

  A connection object to the MultiChain node.

- passphrase:

  Character. The new wallet password.

## Value

Invisibly returns the RPC result (a message indicating the node is
shutting down).

## Warning

MultiChain will shut down after this command is successful. You must
manually restart the node to continue operations.

## See also

Other wallet:
[`mc_backup_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_backup_wallet.md),
[`mc_change_wallet_passphrase()`](https://datascienceadvice.github.io/multichainr/reference/mc_change_wallet_passphrase.md),
[`mc_combine_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_combine_unspent.md),
[`mc_dump_privkey()`](https://datascienceadvice.github.io/multichainr/reference/mc_dump_privkey.md),
[`mc_dump_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_dump_wallet.md),
[`mc_get_wallet_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_wallet_info.md),
[`mc_import_privkey()`](https://datascienceadvice.github.io/multichainr/reference/mc_import_privkey.md),
[`mc_import_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_import_wallet.md),
[`mc_list_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_lock_unspent.md),
[`mc_list_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_unspent.md),
[`mc_lock_unspent()`](https://datascienceadvice.github.io/multichainr/reference/mc_lock_unspent.md),
[`mc_lock_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_lock_wallet.md),
[`mc_unlock_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_unlock_wallet.md)
