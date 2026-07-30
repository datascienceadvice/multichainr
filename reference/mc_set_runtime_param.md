# Set node runtime parameter

Changes a runtime parameter of the node without requiring a restart.
Only a predefined set of parameters can be modified.

## Usage

``` r
mc_set_runtime_param(conn, name, value)
```

## Arguments

- conn:

  A connection object created by
  [`mc_connect`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md).

- name:

  Character string. The name of the parameter to change. Must be one of:

  acceptfiltertimeout

  :   Timeout for filter acceptance.

  autosubscribe

  :   Automatically subscribe to streams.

  bantx

  :   Ban transactions from the mempool.

  handshakelocal

  :   Local handshake behaviour.

  hideknownopdrops

  :   Hide known opdrops.

  lockadminminerounds

  :   Lock admin mining rounds.

  lockblock

  :   Lock block creation.

  lockinlinemetadata

  :   Lock inline metadata.

  maxshowndata

  :   Maximum shown data size.

  maxqueryscanitems

  :   Maximum items to scan in queries.

  mineemptyrounds

  :   Number of empty mining rounds.

  miningrequirespeers

  :   Require peers for mining.

  miningturnover

  :   Mining turnover.

  sendfiltertimeout

  :   Timeout for sending filters.

- value:

  The new value for the parameter. Type depends on the parameter:
  logical, numeric, or character.

## Value

Invisibly returns the RPC result (typically `NULL`) on success; throws
an error if the parameter name is invalid or the value is inappropriate.

## See also

[`mc_get_runtime_params`](https://datascienceadvice.github.io/multichainr/reference/mc_get_runtime_params.md)
to inspect current settings.

Other node configuration:
[`mc_get_blockchain_params()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_blockchain_params.md),
[`mc_get_runtime_params()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_runtime_params.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Turn off auto‑subscription
mc_set_runtime_param(conn, "autosubscribe", FALSE)

# Set maximum connections to 50
mc_set_runtime_param(conn, "maxconnections", 50)
} # }
```
