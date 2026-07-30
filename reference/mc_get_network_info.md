# Get information about node's network status

Returns details about the node's network configuration, including
listening port, local addresses, and network‑related flags.

## Usage

``` r
mc_get_network_info(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

A list containing network information, typically:

- version:

  Node version.

- subversion:

  Node subversion string.

- protocolversion:

  Protocol version.

- localservices:

  Services offered by the node.

- localaddresses:

  List of local IP addresses.

- timeoffset:

  Time offset from network.

- connections:

  Number of active connections.

- relayfee:

  Minimum relay fee.

- ...:

  Other network parameters.

## See also

[`mc_get_peer_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_peer_info.md)
for peer details.

Other networking:
[`mc_add_node()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_node.md),
[`mc_get_added_node_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_added_node_info.md),
[`mc_get_peer_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_peer_info.md),
[`mc_list_stored_nodes()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stored_nodes.md),
[`mc_ping()`](https://datascienceadvice.github.io/multichainr/reference/mc_ping.md),
[`mc_store_node()`](https://datascienceadvice.github.io/multichainr/reference/mc_store_node.md)

## Examples

``` r
if (FALSE) { # \dontrun{
net_info <- mc_get_network_info(conn)
cat("Listening port:", net_info$localaddresses[[1]]$port)
} # }
```
