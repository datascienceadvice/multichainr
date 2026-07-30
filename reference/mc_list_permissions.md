# List network permissions

Returns a list of all permissions (or filtered by type) currently active
on the blockchain.

## Usage

``` r
mc_list_permissions(conn, permissions = "*")
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- permissions:

  Character string. Permission type to filter. Can be a single
  permission name (e.g., `"send"`) or `"*"` (default) to list all
  permissions.

## Value

A data frame (via `rpc_res_to_df`) with permission entries, typically
containing columns like `address`, `type`, `start`, `end`, and `txid`.

## See also

[`mc_grant`](https://datascienceadvice.github.io/multichainr/reference/mc_grant.md),
[`mc_revoke`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke.md).

Other permissions:
[`mc_grant()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant.md),
[`mc_grant_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_from.md),
[`mc_grant_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data.md),
[`mc_grant_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data_from.md),
[`mc_revoke()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke.md),
[`mc_revoke_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke_from.md),
[`mc_verify_permission()`](https://datascienceadvice.github.io/multichainr/reference/mc_verify_permission.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# List all permissions
all_perms <- mc_list_permissions(conn)

# List only "admin" permissions
admins <- mc_list_permissions(conn, "admin")
} # }
```
