# Start a MultiChain node

Launches a MultiChain node for a given blockchain. The node is started
in daemon mode (`-daemon`). If a custom data directory is provided, it
is passed via the `-datadir` argument.

## Usage

``` r
mc_node_start(chain_name, datadir = NULL)
```

## Arguments

- chain_name:

  Character string. Name of the blockchain to start.

- datadir:

  Optional character string. Custom data directory for the blockchain.
  If `NULL` (default), the default MultiChain data location is used.

## Value

Invisibly returns `TRUE` after issuing the start command.

## See also

[`mc_node_init`](https://datascienceadvice.github.io/multichainr/reference/mc_node_init.md)
to create the blockchain,
[`mc_node_stop`](https://datascienceadvice.github.io/multichainr/reference/mc_node_stop.md)
to stop the node.

Other node operations:
[`mc_node_init()`](https://datascienceadvice.github.io/multichainr/reference/mc_node_init.md),
[`mc_node_stop()`](https://datascienceadvice.github.io/multichainr/reference/mc_node_stop.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Start the node for "my_chain"
mc_node_start("my_chain")

# Start with a custom data directory
mc_node_start("my_chain", datadir = "/path/to/data")
} # }
```
