# Get general node information

Returns comprehensive information about the node's status, including
version, protocol, network connections, balance, and mining status.

## Usage

``` r
mc_get_info(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

A list with node information, typically including:

- version:

  Node software version.

- protocolversion:

  Protocol version.

- walletversion:

  Wallet version.

- balance:

  Node's wallet balance.

- blocks:

  Current block height.

- timeoffset:

  Time offset from network.

- connections:

  Number of active connections.

- ...:

  Other status details.

## See also

[`mc_get_blockchain_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_blockchain_info.md)
for blockchain-level info,
[`mc_get_runtime_params`](https://datascienceadvice.github.io/multichainr/reference/mc_get_runtime_params.md)
for runtime settings.

Other node information:
[`mc_get_init_status()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_init_status.md)

## Examples

``` r
if (FALSE) { # \dontrun{
info <- mc_get_info(conn)
cat("Balance:", info$balance, "\n")
cat("Blocks:", info$blocks, "\n")
} # }
```
