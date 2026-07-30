# Create and fund a raw transaction from a specific address

Creates a raw transaction that is automatically funded from a specified
address. This is a convenience function that selects UTXOs from the
given address and builds the transaction.

## Usage

``` r
mc_create_raw_send_from(
  conn,
  from_address,
  to_amounts,
  data = list(),
  action = ""
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- from_address:

  Character string. The address that will fund the transaction.

- to_amounts:

  A named list mapping recipient addresses to amounts (e.g.,
  `list("1A..." = 0.5, "1B..." = list(asset = 10))`).

- data:

  Optional array of metadata (strings or lists; will be hex‑encoded).

- action:

  Optional action: `"send"`, `"sign"`, etc.

## Value

A character string (raw transaction hex) or a list with `hex` and
`complete` if signing is requested.

## See also

[`mc_create_raw_transaction`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_transaction.md)

Other raw transactions:
[`mc_append_raw_change()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_change.md),
[`mc_append_raw_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_data.md),
[`mc_append_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_transaction.md),
[`mc_create_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_transaction.md),
[`mc_decode_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_decode_raw_transaction.md),
[`mc_send_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_raw_transaction.md),
[`mc_sign_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_raw_transaction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Send 1.0 native coin to address
tx_hex <- mc_create_raw_send_from(conn, "1A...", list("1B..." = 1.0))

# Send asset and metadata
tx_hex <- mc_create_raw_send_from(conn, "1A...",
                                  list("1B..." = list(myasset = 50)),
                                  data = list("payment", list(ref = 123)))
} # }
```
