# Get node initialization status

Returns information about the node's initialization progress, especially
useful during startup when the node is still syncing or loading the
wallet.

## Usage

``` r
mc_get_init_status(conn)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

## Value

A list with:

- status:

  Character string describing the current state (e.g.,
  `"Loading blockchain"`, `"Synchronizing"`, `"Ready"`).

- progress:

  Numeric value between 0 and 1 indicating initialization progress (1 =
  fully initialized).

## See also

[`mc_get_info`](https://datascienceadvice.github.io/multichainr/reference/mc_get_info.md)
for general node status.

Other node information:
[`mc_get_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_info.md)

## Examples

``` r
if (FALSE) { # \dontrun{
init <- mc_get_init_status(conn)
while (init$progress < 1) {
  cat("Init progress:", init$progress, "\n")
  Sys.sleep(5)
  init <- mc_get_init_status(conn)
}
} # }
```
