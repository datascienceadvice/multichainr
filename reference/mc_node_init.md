# Initialize a new MultiChain blockchain

Creates a new blockchain using the `multichain-util` command. The new
blockchain is set up in the MultiChain data directory
(platform‑specific).

## Usage

``` r
mc_node_init(chain_name)
```

## Arguments

- chain_name:

  Character string. Name of the blockchain to create.

## Value

Invisibly returns the output of the `multichain-util create` command (a
character vector). If the creation fails, the function stops with an
error.

## See also

[`mc_node_start`](https://datascienceadvice.github.io/multichainr/reference/mc_node_start.md)
to start the created node,
[`mc_node_stop`](https://datascienceadvice.github.io/multichainr/reference/mc_node_stop.md)
to stop it.

Other node operations:
[`mc_node_start()`](https://datascienceadvice.github.io/multichainr/reference/mc_node_start.md),
[`mc_node_stop()`](https://datascienceadvice.github.io/multichainr/reference/mc_node_stop.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a blockchain called "my_chain"
mc_node_init("my_chain")
} # }
```
