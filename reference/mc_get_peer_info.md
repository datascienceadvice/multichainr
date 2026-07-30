# Get information about connected peers

Returns detailed information about each peer currently connected to the
node.

## Usage

``` r
mc_get_peer_info(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

A data frame (via `rpc_res_to_df`) with one row per peer. Common columns
include:

- addr:

  Peer address and port.

- addrlocal:

  Local address used for the connection.

- services:

  Services offered.

- lastsend:

  Last time a message was sent.

- lastrecv:

  Last time a message was received.

- bytessent:

  Total bytes sent.

- bytesrecv:

  Total bytes received.

- conntime:

  Connection start time.

- pingtime:

  Ping time (seconds).

- version:

  Peer's version.

- subver:

  Peer's subversion string.

- inbound:

  Whether the peer connected inbound.

## See also

[`mc_ping`](https://datascienceadvice.github.io/multichainr/reference/mc_ping.md)
to measure latency,
[`mc_get_network_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_network_info.md)
for node network status.

Other networking:
[`mc_add_node()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_node.md),
[`mc_get_added_node_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_added_node_info.md),
[`mc_get_network_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_network_info.md),
[`mc_list_stored_nodes()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stored_nodes.md),
[`mc_ping()`](https://datascienceadvice.github.io/multichainr/reference/mc_ping.md),
[`mc_store_node()`](https://datascienceadvice.github.io/multichainr/reference/mc_store_node.md)

## Examples

``` r
if (FALSE) { # \dontrun{
peers <- mc_get_peer_info(conn)
print(head(peers))
} # }
```
