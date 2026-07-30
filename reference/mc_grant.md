# Grant permissions to an address

Grants one or more permissions to a wallet address. Permissions control
what actions an address can perform on the blockchain (e.g., connect,
send, receive, mine, admin, etc.).

## Usage

``` r
mc_grant(conn, address, permissions)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- address:

  Character string. The address that will receive the permissions.

- permissions:

  Character string. A comma‑separated list of permissions to grant,
  e.g., `"connect,send,receive"`.

## Value

A character string containing the transaction ID (txid) of the grant.

## See also

[`mc_grant_from`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_from.md)
to specify the grantor,
[`mc_revoke`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke.md)
to revoke permissions,
[`mc_list_permissions`](https://datascienceadvice.github.io/multichainr/reference/mc_list_permissions.md)
to view permissions.

Other permissions:
[`mc_grant_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_from.md),
[`mc_grant_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data.md),
[`mc_grant_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data_from.md),
[`mc_list_permissions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_permissions.md),
[`mc_revoke()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke.md),
[`mc_revoke_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke_from.md),
[`mc_verify_permission()`](https://datascienceadvice.github.io/multichainr/reference/mc_verify_permission.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Grant connect and send permissions to an address
txid <- mc_grant(conn, "1A...", "connect,send")
} # }
```
