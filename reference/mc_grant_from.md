# Grant permissions from a specific address

Grants permissions from a specified address (which must have admin or
grant rights). This allows controlling which address pays for the
transaction and acts as the grantor.

## Usage

``` r
mc_grant_from(
  conn,
  from_address,
  to_address,
  permissions,
  native_amount = 0,
  start_block = NULL,
  end_block = NULL
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- from_address:

  Character string. The address granting the permissions.

- to_address:

  Character string. The address receiving the permissions.

- permissions:

  Character string. Comma‑separated list of permissions.

- native_amount:

  Numeric. Amount of native currency to send along with the grant
  (default 0).

- start_block:

  Optional integer. Block height from which the permission becomes
  valid. If `NULL`, the permission starts immediately.

- end_block:

  Optional integer. Block height at which the permission expires. If
  `NULL`, the permission never expires.

## Value

A character string containing the transaction ID.

## See also

[`mc_grant`](https://datascienceadvice.github.io/multichainr/reference/mc_grant.md),
[`mc_revoke_from`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke_from.md).

Other permissions:
[`mc_grant()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant.md),
[`mc_grant_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data.md),
[`mc_grant_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data_from.md),
[`mc_list_permissions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_permissions.md),
[`mc_revoke()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke.md),
[`mc_revoke_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke_from.md),
[`mc_verify_permission()`](https://datascienceadvice.github.io/multichainr/reference/mc_verify_permission.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Grant permissions from a specific admin address
txid <- mc_grant_from(conn, "admin1...", "user1...", "send,receive")

# Grant with a validity window
txid <- mc_grant_from(conn, "admin1...", "user1...", "mine",
                      start_block = 1000, end_block = 2000)
} # }
```
