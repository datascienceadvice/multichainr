# multichainr

**multichainr** is a comprehensive R interface to the
[MultiChain](https://www.multichain.com/) blockchain JSON-RPC API. It
allows you to manage blockchain nodes, create and query data streams,
issue assets, manage permissions, build raw transactions, and much more
– all from within R.

------------------------------------------------------------------------

## Prerequisites

- **R** \>= 3.6.0
- **MultiChain** \>= 2.0 – download from
  [multichain.com/download-install](https://www.multichain.com/download-install/)
- **Pandoc** (only if building vignettes locally)

Make sure the MultiChain binaries (`multichaind`, `multichain-util`) are
accessible. Set the path via:

``` r
mc_set_path("/path/to/multichain/binaries")
```

or add `MULTICHAIN_PATH` to your `.Renviron` file.

------------------------------------------------------------------------

## Installation

``` r
# From GitHub (development version)
remotes::install_github("datascienceadvice/multichainr")

# From CRAN (once published)
install.packages("multichainr")
```

------------------------------------------------------------------------

## Quick Start

``` r
library(multichainr)

# Set path to MultiChain binaries
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

# Create and start a local blockchain
mc_node_init("my_first_chain")
mc_node_start("my_first_chain")

# Connect to the node
config <- mc_get_config("my_first_chain")
conn <- mc_connect(config)

# Check node status
info <- mc_get_info(conn)
cat("Node balance:", info$balance, "\n")
cat("Block height:", info$blocks, "\n")

# Create a stream and publish data
mc_create_stream(conn, "mystream", open = TRUE)
mc_publish(conn, "mystream", "key1", list(text = "Hello, MultiChain!"))

# Query the stream
items <- mc_list_stream_items(conn, "mystream")
print(items)

# Clean up
mc_node_stop(conn)
```

------------------------------------------------------------------------

## Core Features

| Area | Key Functions |
|----|----|
| **Connection** | [`mc_connect()`](https://datascienceadvice.github.io/multichainr/reference/mc_connect.md), [`mc_get_config()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_config.md) |
| **Node Management** | [`mc_node_init()`](https://datascienceadvice.github.io/multichainr/reference/mc_node_init.md), [`mc_node_start()`](https://datascienceadvice.github.io/multichainr/reference/mc_node_start.md), [`mc_node_stop()`](https://datascienceadvice.github.io/multichainr/reference/mc_node_stop.md) |
| **Addresses** | [`mc_get_new_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_new_address.md), [`mc_get_addresses()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_addresses.md), [`mc_validate_address()`](https://datascienceadvice.github.io/multichainr/reference/mc_validate_address.md) |
| **Assets** | [`mc_issue()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue.md), [`mc_issue_from()`](https://datascienceadvice.github.io/multichainr/reference/mc_issue_from.md), [`mc_list_assets()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_assets.md), [`mc_get_asset_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_asset_info.md) |
| **Streams** | [`mc_create_stream()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream.md), [`mc_publish()`](https://datascienceadvice.github.io/multichainr/reference/mc_publish.md), [`mc_list_stream_items()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_stream_items.md) |
| **Permissions** | [`mc_grant()`](https://datascienceadvice.github.io/multichainr/reference/mc_grant.md), [`mc_revoke()`](https://datascienceadvice.github.io/multichainr/reference/mc_revoke.md), [`mc_list_permissions()`](https://datascienceadvice.github.io/multichainr/reference/mc_list_permissions.md) |
| **Payments** | [`mc_send()`](https://datascienceadvice.github.io/multichainr/reference/mc_send.md), [`mc_send_asset()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_asset.md), [`mc_send_with_data()`](https://datascienceadvice.github.io/multichainr/reference/mc_send_with_data.md) |
| **Raw Transactions** | [`mc_create_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_transaction.md), [`mc_sign_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_raw_transaction.md), [`mc_decode_raw_transaction()`](https://datascienceadvice.github.io/multichainr/reference/mc_decode_raw_transaction.md) |
| **Atomic Exchange** | [`mc_create_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_raw_exchange.md), [`mc_append_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_append_raw_exchange.md), [`mc_complete_raw_exchange()`](https://datascienceadvice.github.io/multichainr/reference/mc_complete_raw_exchange.md) |
| **Smart Filters** | [`mc_create_stream_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_stream_filter.md), [`mc_create_tx_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_tx_filter.md), [`mc_test_tx_filter()`](https://datascienceadvice.github.io/multichainr/reference/mc_test_tx_filter.md) |
| **Libraries & Variables** | [`mc_create_library()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_library.md), [`mc_create_variable()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_variable.md), [`mc_set_variable_value()`](https://datascienceadvice.github.io/multichainr/reference/mc_set_variable_value.md) |
| **Subscriptions** | [`mc_subscribe()`](https://datascienceadvice.github.io/multichainr/reference/mc_subscribe.md), [`mc_unsubscribe()`](https://datascienceadvice.github.io/multichainr/reference/mc_unsubscribe.md) |
| **Binary Cache** | [`mc_create_binary_cache()`](https://datascienceadvice.github.io/multichainr/reference/mc_create_binary_cache.md), [`mc_txout_to_binary_cache()`](https://datascienceadvice.github.io/multichainr/reference/mc_txout_to_binary_cache.md) |
| **Network** | [`mc_add_node()`](https://datascienceadvice.github.io/multichainr/reference/mc_add_node.md), [`mc_get_peer_info()`](https://datascienceadvice.github.io/multichainr/reference/mc_get_peer_info.md), [`mc_ping()`](https://datascienceadvice.github.io/multichainr/reference/mc_ping.md) |
| **Messaging** | [`mc_sign_message()`](https://datascienceadvice.github.io/multichainr/reference/mc_sign_message.md), [`mc_verify_message()`](https://datascienceadvice.github.io/multichainr/reference/mc_verify_message.md) |
| **Wallet** | [`mc_backup_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_backup_wallet.md), [`mc_encrypt_wallet()`](https://datascienceadvice.github.io/multichainr/reference/mc_encrypt_wallet.md), [`mc_dump_privkey()`](https://datascienceadvice.github.io/multichainr/reference/mc_dump_privkey.md) |

------------------------------------------------------------------------

## Documentation

Full reference documentation is available at the [package
website](https://datascienceadvice.github.io/multichainr/) (once
published) or via R’s built-in help:

``` r
?mc_connect    # help for a specific function
help(package = "multichainr")  # list all exported functions
```

------------------------------------------------------------------------

## Contributing

Contributions are welcome! Please:

1.  Open an
    [issue](https://github.com/datascienceadvice/multichainr/issues) to
    report bugs or suggest features.
2.  Submit a pull request with a clear description of the change.
3.  Ensure
    [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
    passes before submitting.

------------------------------------------------------------------------

## License

This project is licensed under the MIT License – see the
[LICENSE](https://datascienceadvice.github.io/multichainr/LICENSE) file
for details.
