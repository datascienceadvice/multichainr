# Grant permissions with metadata

Grants permissions and attaches arbitrary data (metadata) to the
transaction. The data can be text, JSON, or any hex‑encoded value.

## Usage

``` r
mc_grant_with_data(conn, to_address, permissions, data, native_amount = 0)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- to_address:

  Character string. The address receiving the permissions.

- permissions:

  Character string. Comma‑separated list of permissions.

- data:

  Data to embed. Can be a character string (will be hex‑encoded), a list
  (converted to JSON then hex), or raw binary.

- native_amount:

  Numeric. Amount of native currency to send (default 0).

## Value

A character string containing the transaction ID.

## See also

[`mc_grant_with_data_from`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data_from.md)
to specify the grantor.

Other permissions:
[`mc_grant()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant.md),
[`mc_grant_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_from.md),
[`mc_grant_with_data_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant_with_data_from.md),
[`mc_list_permissions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_permissions.md),
[`mc_revoke()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke.md),
[`mc_revoke_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke_from.md),
[`mc_verify_permission()`](https://datascienceadvice.github.io/multichainr/reference/mc_verify_permission.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Grant with a text note
txid <- mc_grant_with_data(conn, "1A...", "send",
                           data = "Welcome to the network!")

# Grant with JSON metadata
metadata <- list(reason = "partnership", level = "full")
txid <- mc_grant_with_data(conn, "1A...", "connect,send",
                           data = metadata, native_amount = 0.1)
} # }
```
