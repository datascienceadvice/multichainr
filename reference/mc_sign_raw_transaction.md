# Sign a raw transaction

Signs a raw transaction using the node's wallet or provided private
keys. Returns the signed hex and a boolean indicating whether all inputs
are signed.

## Usage

``` r
mc_sign_raw_transaction(
  conn,
  tx_hex,
  parents = NULL,
  private_keys = NULL,
  sighashtype = "ALL"
)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- tx_hex:

  Character string. The raw transaction hex to sign.

- parents:

  Optional list of parent outputs for signing, each containing `txid`,
  `vout`, and `scriptPubKey`.

- private_keys:

  Optional character vector of private keys in Wallet Import Format
  (WIF). If provided, these are used instead of the node's wallet.

- sighashtype:

  Character string. Signature hash type (default `"ALL"`).

## Value

A list with two elements:

- hex:

  The signed raw transaction hex (if all inputs are signed).

- complete:

  Logical; `TRUE` if all inputs are signed.

## See also

[`mc_send_raw_transaction`](https://datascienceadvice.github.io/multichainr/reference/mc_send_raw_transaction.md),
[`mc_create_raw_transaction`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_transaction.md)

Other raw transactions:
[`mc_append_raw_change()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_change.md),
[`mc_append_raw_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_data.md),
[`mc_append_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_transaction.md),
[`mc_create_raw_send_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_send_from.md),
[`mc_create_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_transaction.md),
[`mc_decode_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_decode_raw_transaction.md),
[`mc_send_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_raw_transaction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Sign using the node's wallet
signed <- mc_sign_raw_transaction(conn, tx_hex)

# Sign with explicit private keys
signed <- mc_sign_raw_transaction(conn, tx_hex,
                                  private_keys = c("L5...", "K3..."))
} # }
```
