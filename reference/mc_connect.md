# Create a MultiChain connection object

Establishes a connection to a MultiChain node by constructing an RPC
endpoint. The function accepts either explicit parameters (host, port,
user, password) or a configuration list (typically obtained from
[`mc_get_config`](https://datascienceadvice.github.io/multichainr/reference/mc_get_config.md)).

## Usage

``` r
mc_connect(host = "127.0.0.1", port = NULL, user = NULL, password = NULL)
```

## Arguments

- host:

  Either a character string with the IP address or hostname of the
  MultiChain node, or a configuration list (as returned by
  [`mc_get_config`](https://datascienceadvice.github.io/multichainr/reference/mc_get_config.md))
  containing `host`, `port`, `user`, and `password`. When a list is
  provided, the other arguments are ignored.

- port:

  Integer. RPC port number. Required unless `host` is a list.

- user:

  Character string. RPC username. Required unless `host` is a list.

- password:

  Character string. RPC password. Required unless `host` is a list.

## Value

An object of class `"multichain_conn"` containing the RPC URL, username,
and password (the password is stored but hidden in printing).

## See also

[`mc_get_config`](https://datascienceadvice.github.io/multichainr/reference/mc_get_config.md)
to obtain a configuration list,
[`print.multichain_conn`](https://datascienceadvice.github.io/multichainr/reference/print.multichain_conn.md)
for printing connections.

## Examples

``` r
if (FALSE) { # \dontrun{
# Using explicit parameters
conn <- mc_connect(host = "127.0.0.1", port = 8570,
                   user = "multichainrpc", password = "mysecret")

# Using a configuration object from mc_get_config
config <- mc_get_config("my_chain")
conn <- mc_connect(config)
} # }
```
