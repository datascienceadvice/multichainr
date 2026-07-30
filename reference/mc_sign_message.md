# Sign a message with a private key

Generates a base64‑encoded digital signature for a message using a
private key. The signature proves that the message was approved by the
owner of the address or the holder of the private key.

## Usage

``` r
mc_sign_message(conn, address_or_key, message)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- address_or_key:

  Character string. Either a wallet address (must belong to the node's
  wallet) or a private key in Wallet Import Format (WIF).

- message:

  Character string. The text message to sign.

## Value

A character string containing the base64‑encoded signature.

## See also

[`mc_verify_message`](https://datascienceadvice.github.io/multichainr/reference/mc_verify_message.md)
to verify a signature,
[`mc_get_new_address`](https://datascienceadvice.github.io/multichainr/reference/mc_get_new_address.md)
to generate a new address.

Other cryptography:
[`mc_verify_message()`](https://datascienceadvice.github.io/multichainr/reference/mc_verify_message.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Sign using an address in the wallet
sig <- mc_sign_message(conn, "1A...", "Hello, MultiChain!")

# Sign using a raw private key
sig <- mc_sign_message(conn, "L5...", "Important agreement")
} # }
```
