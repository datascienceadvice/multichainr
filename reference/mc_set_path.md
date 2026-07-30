# Set path to MultiChain binaries

This function sets the global option `multichain.path` to the directory
containing the MultiChain executables (`multichaind` and
`multichain-util`). All other functions that need to locate the binaries
will use this option.

## Usage

``` r
mc_set_path(path)
```

## Arguments

- path:

  Character string. Path to the folder containing the MultiChain
  executables. Must be an existing directory.

## Value

Invisibly returns the normalized path (as set in the option) or throws
an error if the directory does not exist.

## See also

[`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md)
for establishing a connection.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set path to MultiChain installation (example on Unix-like systems)
mc_set_path("/usr/local/bin")

# Check that the option was set correctly
getOption("multichain.path")
} # }
```
