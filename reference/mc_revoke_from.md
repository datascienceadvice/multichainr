# Revoke permissions from a specific address

Revokes permissions from an address, specifying the address that issues
the revocation. This allows controlling which address pays for the
transaction.

## Usage

``` r
mc_revoke_from(conn, from_address, to_address, permissions, native_amount = 0)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- from_address:

  Character string. The address revoking the permissions.

- to_address:

  Character string. The address losing the permissions.

- permissions:

  Character string. Comma‑separated list of permissions to revoke.

- native_amount:

  Numeric. Amount of native currency to send along with the revocation
  (default 0).

## Value

A character string containing the transaction ID.

## See also

[`mc_revoke`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke.md),
[`mc_grant_from`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_from.md).

Other permissions:
[`mc_grant()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant.md),
[`mc_grant_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_from.md),
[`mc_grant_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data.md),
[`mc_grant_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data_from.md),
[`mc_list_permissions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_permissions.md),
[`mc_revoke()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke.md),
[`mc_verify_permission()`](https://datascienceadvice.github.io/multichainr/reference/mc_verify_permission.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Revoke from a specific admin address
txid <- mc_revoke_from(conn, "admin1...", "user1...", "send")
} # }
```
