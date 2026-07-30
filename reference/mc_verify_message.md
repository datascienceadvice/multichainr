# Verify a signed message

Checks whether a message was signed by the owner of a given address. The
signature must have been created by
[`mc_sign_message`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_message.md).

## Usage

``` r
mc_verify_message(conn, address, signature, message)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- address:

  Character string. The address that allegedly signed the message.

- signature:

  Character string. The base64‑encoded signature (as returned by
  [`mc_sign_message`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_message.md)).

- message:

  Character string. The original text message.

## Value

A logical value: `TRUE` if the signature is valid, `FALSE` otherwise.

## See also

[`mc_sign_message`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_message.md)
to create a signature.

Other cryptography:
[`mc_sign_message()`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_message.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Sign a message
sig <- mc_sign_message(conn, "1A...", "Hello")
# Verify it
valid <- mc_verify_message(conn, "1A...", sig, "Hello")
print(valid)  # should be TRUE
} # }
```
