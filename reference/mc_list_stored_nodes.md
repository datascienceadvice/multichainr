# List known peer node addresses (MultiChain 2.3+)

Returns a list of nodes that the node has stored in its address manager.
This includes peers that were connected to or manually added.

## Usage

``` r
mc_list_stored_nodes(conn, include_old_ignores = FALSE)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- include_old_ignores:

  Logical. If `TRUE`, includes nodes that were previously ignored.
  Default is `FALSE`.

## Value

A data frame (via `rpc_res_to_df`) of stored nodes.

## See also

[`mc_store_node`](https://datascienceadvice.github.io/multichainr/reference/mc_store_node.md)
to add a node to the stored list.

Other networking:
[`mc_add_node()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_node.md),
[`mc_get_added_node_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_added_node_info.md),
[`mc_get_network_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_network_info.md),
[`mc_get_peer_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_peer_info.md),
[`mc_ping()`](https://datascienceadvice.github.io/multichainr/reference/mc_ping.md),
[`mc_store_node()`](https://datascienceadvice.github.io/multichainr/reference/mc_store_node.md)

## Examples

``` r
if (FALSE) { # \dontrun{
nodes <- mc_list_stored_nodes(conn)
} # }
```
