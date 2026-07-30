# Stop a MultiChain node

Stops a running MultiChain node. The function accepts either a
connection object (created by
[`mc_connect()`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md))
or a chain name. When a chain name is provided, it first retrieves the
configuration and establishes a connection automatically.

## Usage

``` r
mc_node_stop(x)
```

## Arguments

- x:

  Either:

  - A character string (chain name) — automatically retrieves the
    configuration and establishes a connection before stopping.

  - An object of class `"multichain_conn"` — sends the stop command
    directly to that connected node.

## Value

Invisibly returns the result of the RPC `stop` command.

## See also

[`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md),
[`mc_node_start`](https://datascienceadvice.github.io/multichainr/reference/mc_node_start.md)
to start a node.

Other node operations:
[`mc_node_init()`](https://datascienceadvice.github.io/multichainr/reference/mc_node_init.md),
[`mc_node_start()`](https://datascienceadvice.github.io/multichainr/reference/mc_node_start.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Stop by chain name
mc_node_stop("my_chain")

# Stop using a connection object
conn <- mc_connect(mc_get_config("my_chain"))
mc_node_stop(conn)
} # }
```
