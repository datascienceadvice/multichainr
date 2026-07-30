# Get MultiChain configuration

Reads the configuration parameters (RPC user, password, port) for a
given blockchain from the MultiChain data directory. The function
automatically determines the platform‑specific base directory, but a
custom base can be supplied for testing.

## Usage

``` r
mc_get_config(chain_name, base_dir = NULL)
```

## Arguments

- chain_name:

  Character string. Name of the MultiChain blockchain.

- base_dir:

  Optional character string. Base directory where MultiChain stores
  blockchain data. If `NULL` (default), the platform‑specific default is
  used:

  - Windows: `%APPDATA%/MultiChain`

  - macOS: `~/Library/Application Support/MultiChain`

  - Linux/other: `~/.multichain`

## Value

A list with four components:

- user:

  RPC username (from `multichain.conf`).

- password:

  RPC password (from `multichain.conf`).

- port:

  RPC port number (integer).

- host:

  Always `"127.0.0.1"` (hard‑coded).

## See also

[`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md)
to create a connection object using the returned configuration.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get configuration for a chain called "my_chain"
config <- mc_get_config("my_chain")
print(config)
} # }
```
