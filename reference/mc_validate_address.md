# Validate or inspect an address

Returns information about a given address, private key, or public key.
Useful for checking whether an address is valid, whether it belongs to
the current node, and for inspecting its associated redeem script.

## Usage

``` r
mc_validate_address(conn, address_or_key)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- address_or_key:

  Character string. An address, private key, or public key.

## Value

A list with information about the input, including:

- isvalid:

  Logical indicating whether the input is valid.

- address:

  The canonical address (if valid).

- ismine:

  Logical indicating whether the address belongs to the node.

- ...:

  Other fields depending on the input type (e.g., pubkey, script).

## See also

[`mc_import_address`](https://datascienceadvice.github.io/multichainr/reference/mc_import_address.md)
to add an address to the wallet.

Other addresses:
[`mc_add_multisig_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_multisig_address.md),
[`mc_create_keypairs()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_keypairs.md),
[`mc_create_multisig()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_multisig.md),
[`mc_get_addresses()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_addresses.md),
[`mc_get_new_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_new_address.md),
[`mc_import_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_import_address.md),
[`mc_list_addresses()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_addresses.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Validate an address
info <- mc_validate_address(conn, "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa")
print(info$isvalid)

# Validate a private key (if known)
key_info <- mc_validate_address(conn, "L5...")
} # }
```
