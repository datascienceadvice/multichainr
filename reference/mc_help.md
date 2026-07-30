# Get help for MultiChain commands

Returns a list of all available RPC commands, or detailed help for a
specific command. The result is printed in a human‑readable format.

## Usage

``` r
mc_help(conn, command = NULL)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- command:

  Optional character string. The name of the command to get detailed
  help for. If `NULL` (default), returns a list of all commands.

## Value

An object of class `"mc_help"` (inheriting from `"character"`) that
contains the help text and prints nicely via
[`print.mc_help`](https://datascienceadvice.github.io/multichainr/reference/print.mc_help.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# List all available commands
mc_help(conn)

# Get detailed help for the "getinfo" command
mc_help(conn, "getinfo")
} # }

```
