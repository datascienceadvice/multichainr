# Revoke permissions from an address

Removes one or more permissions from a wallet address. The revoking
address must have admin rights.

## Usage

``` r
mc_revoke(conn, address, permissions)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- address:

  Character string. The address from which permissions are revoked.

- permissions:

  Character string. Comma‑separated list of permissions to revoke.

## Value

A character string containing the transaction ID.

## See also

[`mc_revoke_from`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke_from.md)
to specify the revoker,
[`mc_grant`](https://datascienceadvice.github.io/multichainr/reference/mc_grant.md)
for granting.

Other permissions:
[`mc_grant()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant.md),
[`mc_grant_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_from.md),
[`mc_grant_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data.md),
[`mc_grant_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data_from.md),
[`mc_list_permissions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_permissions.md),
[`mc_revoke_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke_from.md),
[`mc_verify_permission()`](https://datascienceadvice.github.io/multichainr/reference/mc_verify_permission.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Revoke send and receive permissions
txid <- mc_revoke(conn, "1A...", "send,receive")
} # }
```
