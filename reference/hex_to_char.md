# Decode hex string to character

Converts a hexadecimal encoded string back into its original character
representation. This is primarily used for reading human-readable data
published to MultiChain streams.

## Usage

``` r
hex_to_char(hex_str)
```

## Arguments

- hex_str:

  A character string in hexadecimal format.

## Value

A decoded character string. If the input is not a valid hex string or an
error occurs during decoding, the original `hex_str` is returned.

## Details

The function performs a basic validation to ensure the string is a valid
hexadecimal representation (even length and containing only hex
characters) before attempting to convert.

## Examples

``` r
hex_to_char("48656c6c6f")  # Returns "Hello"
#> [1] "Hello"
```
