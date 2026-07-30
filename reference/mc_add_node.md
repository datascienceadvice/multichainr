# Add or remove a peer-to-peer connection

Manages the node's peer connections. Can add a node to the connection
queue, remove an existing connection, or attempt a one‑time connection.

## Usage

``` r
mc_add_node(conn, node, command = c("add", "remove", "onetry"))
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- node:

  Character string. The IP address and port of the peer node, e.g.,
  `"127.0.0.1:8571"`.

- command:

  Character string. The action to perform:

  - `"add"` – add the node to the connection queue (will attempt to
    connect and stay connected).

  - `"remove"` – disconnect from the node if currently connected.

  - `"onetry"` – attempt a single connection; do not keep retrying.

## Value

Invisibly returns the RPC result (typically `NULL`) on success; throws
an error if the command fails.

## See also

[`mc_get_added_node_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_added_node_info.md)
to list added nodes,
[`mc_get_peer_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_peer_info.md)
for connected peers.

Other networking:
[`mc_get_added_node_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_added_node_info.md),
[`mc_get_network_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_network_info.md),
[`mc_get_peer_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_peer_info.md),
[`mc_list_stored_nodes()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stored_nodes.md),
[`mc_ping()`](https://datascienceadvice.github.io/multichainr/reference/mc_ping.md),
[`mc_store_node()`](https://datascienceadvice.github.io/multichainr/reference/mc_store_node.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Add a peer
mc_add_node(conn, "192.168.1.10:8571", command = "add")

# Remove a peer
mc_add_node(conn, "192.168.1.10:8571", command = "remove")
} # }
```
