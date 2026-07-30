# Verify if an address has a specific permission

Checks whether a given address has a particular permission on the
blockchain.

## Usage

``` r
mc_verify_permission(conn, address, permission)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- address:

  Character string. The address to check.

- permission:

  Character string. The permission name (e.g., `"send"`, `"admin"`,
  `"connect"`).

## Value

A logical value: `TRUE` if the address has the permission, `FALSE`
otherwise.

## See also

[`mc_list_permissions`](https://datascienceadvice.github.io/multichainr/reference/mc_list_permissions.md)
to see all permissions.

Other permissions:
[`mc_grant()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant.md),
[`mc_grant_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_from.md),
[`mc_grant_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data.md),
[`mc_grant_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data_from.md),
[`mc_list_permissions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_permissions.md),
[`mc_revoke()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke.md),
[`mc_revoke_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke_from.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Check if an address can send
can_send <- mc_verify_permission(conn, "1A...", "send")
if (can_send) cat("Address can send assets")
} # }
```
