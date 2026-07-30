# Subscribe to MultiChain assets or streams

Instructs the MultiChain node to start tracking one or more asset(s) or
stream(s). This is often required before you can retrieve items or
balances for specific entities.

## Usage

``` r
mc_subscribe(conn, entities, rescan = TRUE)
```

## Arguments

- conn:

  A connection object to the MultiChain node (typically created via
  `mc_connect`).

- entities:

  A character string or vector of strings representing asset/stream
  names, references, or transaction IDs (txids).

- rescan:

  Logical. If `TRUE` (default), the node reindexes all items from the
  point of creation of the entities. If `FALSE`, only new items will be
  tracked.

## Value

Returns `NULL` invisibly on success, or an error if the RPC call fails.

## See also

[`mc_unsubscribe`](https://datascienceadvice.github.io/multichainr/reference/mc_unsubscribe.md)

Other subscriptions:
[`mc_unsubscribe()`](https://datascienceadvice.github.io/multichainr/reference/mc_unsubscribe.md)
