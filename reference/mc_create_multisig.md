# Create multi-signature address (external)

Creates a Pay‑to‑Script‑Hash (P2SH) multi-signature address without
adding it to the wallet. The address can be used in transactions that
require multiple signatures, but the node cannot spend funds from it
unless the private keys are also imported.

## Usage

``` r
mc_create_multisig(conn, n_required, keys)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- n_required:

  Integer. Number of required signatures.

- keys:

  Character vector. Public keys or addresses.

## Value

A list with two components:

- address:

  The multi-signature address.

- redeemScript:

  The redeem script (needed for spending).

## See also

[`mc_add_multisig_address`](https://datascienceadvice.github.io/multichainr/reference/mc_add_multisig_address.md)
to create and add the address to the wallet.

Other addresses:
[`mc_add_multisig_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_multisig_address.md),
[`mc_create_keypairs()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_keypairs.md),
[`mc_get_addresses()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_addresses.md),
[`mc_get_new_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_new_address.md),
[`mc_import_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_import_address.md),
[`mc_list_addresses()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_addresses.md),
[`mc_validate_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_validate_address.md)

## Examples

``` r
if (FALSE) { # \dontrun{
multisig <- mc_create_multisig(conn, n_required = 2,
                               keys = c("pubkey1", "pubkey2", "pubkey3"))
cat("Multisig address:", multisig$address)
} # }
```
