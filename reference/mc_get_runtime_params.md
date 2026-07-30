# Get node runtime parameters

Returns the current runtime parameters of the node. These can be changed
while the node is running (see
[`mc_set_runtime_param`](https://datascienceadvice.github.io/multichainr/reference/mc_set_runtime_param.md)).

## Usage

``` r
mc_get_runtime_params(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

A list of runtime parameters, e.g.:

- mining:

  Logical; whether mining is enabled.

- maxconnections:

  Maximum number of inbound connections.

- ...:

  Other runtime settings.

## See also

[`mc_set_runtime_param`](https://datascienceadvice.github.io/multichainr/reference/mc_set_runtime_param.md)
to modify parameters,
[`mc_get_blockchain_params`](https://datascienceadvice.github.io/multichainr/reference/mc_get_blockchain_params.md)
for fixed chain parameters.

Other node configuration:
[`mc_get_blockchain_params()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_blockchain_params.md),
[`mc_set_runtime_param()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_runtime_param.md)

## Examples

``` r
if (FALSE) { # \dontrun{
runtime <- mc_get_runtime_params(conn)
print(runtime$mining)
} # }
```
