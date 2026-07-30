# Get node wallet addresses

Returns all addresses owned by the current node. If `verbose = TRUE`,
detailed information (including balances and transactions) is returned.

## Usage

``` r
mc_get_addresses(conn, verbose = FALSE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- verbose:

  Logical. If `TRUE`, returns a list with detailed information for each
  address. If `FALSE` (default), returns a character vector of
  addresses.

## Value

If `verbose = FALSE`: a character vector of addresses. If
`verbose = TRUE`: a list (or data frame) with details.

## See also

[`mc_list_addresses`](https://datascienceadvice.github.io/multichainr/reference/mc_list_addresses.md)
for more flexible listing options.

Other addresses:
[`mc_add_multisig_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_multisig_address.md),
[`mc_create_keypairs()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_keypairs.md),
[`mc_create_multisig()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_multisig.md),
[`mc_get_new_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_new_address.md),
[`mc_import_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_import_address.md),
[`mc_list_addresses()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_addresses.md),
[`mc_validate_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_validate_address.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all addresses (simple list)
addresses <- mc_get_addresses(conn)

# Get detailed information
details <- mc_get_addresses(conn, verbose = TRUE)
} # }
```
