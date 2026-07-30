# Print MultiChain connection

S3 method for printing `multichain_conn` objects. Hides the password for
security.

## Usage

``` r
# S3 method for class 'multichain_conn'
print(x, ...)
```

## Arguments

- x:

  An object of class `"multichain_conn"`.

- ...:

  Additional arguments passed to `print` (ignored).

## Value

Invisibly returns the object `x`.

## See also

[`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md)
for creating connections.

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- mc_connect(config)
print(conn)   # or simply conn
} # }
```
