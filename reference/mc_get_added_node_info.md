# Get information about manually added nodes

Returns details about nodes that were added via
[`mc_add_node`](https://datascienceadvice.github.io/multichainr/reference/mc_add_node.md).
Can return either a list of node addresses or detailed information.

## Usage

``` r
mc_get_added_node_info(conn, verbose = FALSE, node = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- verbose:

  Logical. If `TRUE`, returns a data frame with detailed information
  about each added node (e.g., connected status, last connection time).
  If `FALSE` (default), returns a character vector of node addresses.

- node:

  Optional character string. If provided, returns information only for
  that specific node.

## Value

If `verbose = FALSE`: a character vector of node addresses. If
`verbose = TRUE`: a data frame (via `rpc_res_to_df`) with node details.

## See also

[`mc_add_node`](https://datascienceadvice.github.io/multichainr/reference/mc_add_node.md),
[`mc_get_peer_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_peer_info.md)

Other networking:
[`mc_add_node()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_node.md),
[`mc_get_network_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_network_info.md),
[`mc_get_peer_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_peer_info.md),
[`mc_list_stored_nodes()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stored_nodes.md),
[`mc_ping()`](https://datascienceadvice.github.io/multichainr/reference/mc_ping.md),
[`mc_store_node()`](https://datascienceadvice.github.io/multichainr/reference/mc_store_node.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# List all added nodes (addresses only)
nodes <- mc_get_added_node_info(conn)

# Get detailed information for a specific node
details <- mc_get_added_node_info(conn, verbose = TRUE, node = "192.168.1.10:8571")
} # }
```
