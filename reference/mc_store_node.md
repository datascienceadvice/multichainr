# Add an IP address to known peer nodes (MultiChain 2.3+)

Stores a node address in the node's address manager. This can be used to
manually add a peer for future connections or to ignore a node.

## Usage

``` r
mc_store_node(conn, node, command = c("tryconnect", "ignore"))
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- node:

  Character string. The IP address and port of the peer node.

- command:

  Character string. Action to perform:

  - `"tryconnect"` – store the node and try to connect to it (default).

  - `"ignore"` – ignore the node (do not attempt connections).

## Value

Invisibly returns the RPC result (typically `NULL`) on success.

## See also

[`mc_list_stored_nodes`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stored_nodes.md)
to list stored nodes.

Other networking:
[`mc_add_node()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_node.md),
[`mc_get_added_node_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_added_node_info.md),
[`mc_get_network_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_network_info.md),
[`mc_get_peer_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_peer_info.md),
[`mc_list_stored_nodes()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stored_nodes.md),
[`mc_ping()`](https://datascienceadvice.github.io/multichainr/reference/mc_ping.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Add a node to the address manager
mc_store_node(conn, "192.168.1.20:8571", command = "tryconnect")
} # }
```
