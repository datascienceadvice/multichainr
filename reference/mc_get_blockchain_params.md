# Get blockchain parameters

Returns the parameters that were used to initialize this blockchain.
These are fixed at chain creation and cannot be changed later.

## Usage

``` r
mc_get_blockchain_params(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

A list containing blockchain configuration parameters, such as:

- protocolversion:

  Protocol version.

- targetblocktime:

  Target time between blocks (seconds).

- maxblocksize:

  Maximum block size (bytes).

- ...:

  Other chain-specific parameters.

## See also

[`mc_get_runtime_params`](https://datascienceadvice.github.io/multichainr/reference/mc_get_runtime_params.md)
for modifiable parameters.

Other node configuration:
[`mc_get_runtime_params()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_runtime_params.md),
[`mc_set_runtime_param()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_runtime_param.md)

## Examples

``` r
if (FALSE) { # \dontrun{
params <- mc_get_blockchain_params(conn)
print(params$targetblocktime)
} # }
```
