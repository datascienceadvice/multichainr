# Unsubscribe from MultiChain assets or streams

Instructs the MultiChain node to stop tracking one or more asset(s) or
stream(s).

## Usage

``` r
mc_unsubscribe(conn, entities, purge = FALSE)
```

## Arguments

- conn:

  A connection object to the MultiChain node.

- entities:

  A character string or vector of strings representing asset/stream
  names, references, or transaction IDs (txids).

- purge:

  Logical. If `TRUE`, any off-chain data retrieved for this stream will
  be permanently purged from the node's local storage. Defaults to
  `FALSE`.

## Value

Returns `NULL` invisibly on success, or an error if the RPC call fails.

## See also

[`mc_subscribe`](https://datascienceadvice.github.io/multichainr/reference/mc_subscribe.md)

Other subscriptions:
[`mc_subscribe()`](https://datascienceadvice.github.io/multichainr/reference/mc_subscribe.md)
