# Add a multi-signature address

Creates a multi-signature address and adds it to the node's wallet. The
address is a Pay‑to‑Script‑Hash (P2SH) address that requires a specified
number of signatures from the provided keys.

## Usage

``` r
mc_add_multisig_address(conn, n_required, keys)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- n_required:

  Integer. Number of signatures required to spend funds.

- keys:

  Character vector. Public keys or addresses that will be part of the
  multi-signature set.

## Value

A character string containing the multi-signature address.

## See also

[`mc_create_multisig`](https://datascienceadvice.github.io/multichainr/reference/mc_create_multisig.md)
to create a multisig address without adding it to the wallet.

Other addresses:
[`mc_create_keypairs()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_keypairs.md),
[`mc_create_multisig()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_multisig.md),
[`mc_get_addresses()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_addresses.md),
[`mc_get_new_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_new_address.md),
[`mc_import_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_import_address.md),
[`mc_list_addresses()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_addresses.md),
[`mc_validate_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_validate_address.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Assume connection 'conn' is already established
addr <- mc_add_multisig_address(conn, n_required = 2,
                                keys = c("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa",
                                         "1BvBMSEYstWetqTFn5Au4m4GFg7xJaNVN2"))
} # }
```
