# Wait for transaction confirmation

Blocks execution until a transaction is included in a block.

## Usage

``` r
mc_wait_for_confirmation(conn, txid, timeout = 30)
```

## Arguments

- conn:

  A connection object.

- txid:

  Character string. Transaction ID.

- timeout:

  Integer. Maximum time to wait in seconds (default 30).

## Value

Logical TRUE if confirmed, throws error if timeout reached.
