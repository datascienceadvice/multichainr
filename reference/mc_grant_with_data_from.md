# Grant permissions from a specific address with metadata

Grants permissions from a specified address and includes metadata.
Combines the capabilities of
[`mc_grant_from`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_from.md)
and
[`mc_grant_with_data`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data.md).

## Usage

``` r
mc_grant_with_data_from(
  conn,
  from_address,
  to_address,
  permissions,
  data,
  native_amount = 0
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

- data:

  Data to embed (string or list, automatically hex‑encoded).

- native_amount:

  Numeric. Amount of native currency to send (default 0).

## Value

A character string containing the transaction ID.

## See also

[`mc_grant_with_data`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data.md),
[`mc_grant_from`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_from.md).

Other permissions:
[`mc_grant()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant.md),
[`mc_grant_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_from.md),
[`mc_grant_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data.md),
[`mc_list_permissions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_permissions.md),
[`mc_revoke()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke.md),
[`mc_revoke_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke_from.md),
[`mc_verify_permission()`](https://datascienceadvice.github.io/multichainr/reference/mc_verify_permission.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Grant from a specific admin with metadata
txid <- mc_grant_with_data_from(conn, "admin1...", "user1...",
                                "send,receive",
                                data = list(note = "temporary access"),
                                native_amount = 0.01)
} # }
```
