# Ping all connected peers

Sends a ping message to all connected peers. The ping time (latency) is
recorded and can be viewed in the `pingtime` column of
[`mc_get_peer_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_peer_info.md).

## Usage

``` r
mc_ping(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

Invisibly returns the RPC result (typically `NULL`) on success.

## See also

[`mc_get_peer_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_peer_info.md)
to view ping times.

Other networking:
[`mc_add_node()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_node.md),
[`mc_get_added_node_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_added_node_info.md),
[`mc_get_network_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_network_info.md),
[`mc_get_peer_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_peer_info.md),
[`mc_list_stored_nodes()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stored_nodes.md),
[`mc_store_node()`](https://datascienceadvice.github.io/multichainr/reference/mc_store_node.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mc_ping(conn)
Sys.sleep(1)
peers <- mc_get_peer_info(conn)
print(peers$pingtime)
} # }
```
