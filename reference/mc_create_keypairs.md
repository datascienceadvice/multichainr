# Create new key pairs

Generates one or more public/private key pairs. These keys are *not*
stored in the node's wallet, so they must be kept secure by the user.

## Usage

``` r
mc_create_keypairs(conn, count = 1)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- count:

  Integer. Number of key pairs to generate. Default is `1`.

## Value

A data frame with three columns:

- address:

  The public address derived from the key pair.

- pubkey:

  The public key.

- privkey:

  The private key (keep this secret!).

## See also

[`mc_get_new_address`](https://datascienceadvice.github.io/multichainr/reference/mc_get_new_address.md)
to create an address stored in the wallet.

Other addresses:
[`mc_add_multisig_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_multisig_address.md),
[`mc_create_multisig()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_multisig.md),
[`mc_get_addresses()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_addresses.md),
[`mc_get_new_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_new_address.md),
[`mc_import_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_import_address.md),
[`mc_list_addresses()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_addresses.md),
[`mc_validate_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_validate_address.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate a single key pair
keys <- mc_create_keypairs(conn)
print(keys)

# Generate 5 key pairs
keys5 <- mc_create_keypairs(conn, count = 5)
} # }
```
