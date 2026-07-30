# Import a watch-only address

Adds an address (without a private key) to the node's wallet for
monitoring. The node will be able to see transactions involving this
address, but cannot spend funds from it.

## Usage

``` r
mc_import_address(conn, address, label = "", rescan = TRUE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- address:

  Character string. The wallet address to import.

- label:

  Character string (optional). A label to assign to the address. Default
  is `""` (no label).

- rescan:

  Logical. If `TRUE` (default), the node will scan the blockchain for
  transactions associated with this address.

## Value

Invisibly returns the RPC result (typically `NULL`) on success; throws
an error if the import fails.

## See also

[`mc_validate_address`](https://datascienceadvice.github.io/multichainr/reference/mc_validate_address.md)
to check address validity.

Other addresses:
[`mc_add_multisig_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_multisig_address.md),
[`mc_create_keypairs()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_keypairs.md),
[`mc_create_multisig()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_multisig.md),
[`mc_get_addresses()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_addresses.md),
[`mc_get_new_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_new_address.md),
[`mc_list_addresses()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_addresses.md),
[`mc_validate_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_validate_address.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mc_import_address(conn, "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa",
                  label = "donation", rescan = TRUE)
} # }
```
