# List addresses in the wallet

Returns information about the addresses in the current node's wallet.
This is a more flexible version of
[`mc_get_addresses`](https://datascienceadvice.github.io/multichainr/reference/mc_get_addresses.md),
allowing filtering, pagination, and optional verbosity.

## Usage

``` r
mc_list_addresses(
  conn,
  addresses = "*",
  verbose = FALSE,
  count = NULL,
  start = NULL
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- addresses:

  A character vector of addresses to filter, or `"*"` (default) to list
  all addresses.

- verbose:

  Logical. If `TRUE`, returns detailed information for each address.

- count:

  Integer (optional). Maximum number of addresses to return.

- start:

  Integer (optional). Offset for pagination.

## Value

A data frame (created by `rpc_res_to_df`) containing address
information. The exact columns depend on the `verbose` setting, but
typically include `address`, `label`, `balance`, etc.

## See also

[`mc_get_addresses`](https://datascienceadvice.github.io/multichainr/reference/mc_get_addresses.md)
for a simpler version.

Other addresses:
[`mc_add_multisig_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_multisig_address.md),
[`mc_create_keypairs()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_keypairs.md),
[`mc_create_multisig()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_multisig.md),
[`mc_get_addresses()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_addresses.md),
[`mc_get_new_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_new_address.md),
[`mc_import_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_import_address.md),
[`mc_validate_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_validate_address.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# List all addresses (simple)
all_addr <- mc_list_addresses(conn)

# List detailed information for specific addresses
details <- mc_list_addresses(conn,
                             addresses = c("1A...", "1B..."),
                             verbose = TRUE)

# Paginate results
first_10 <- mc_list_addresses(conn, count = 10)
next_10  <- mc_list_addresses(conn, count = 10, start = 10)
} # }
```
