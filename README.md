# multichainr

<!-- badges: start -->
[![R-CMD-check](https://github.com/datascienceadvice/multichainr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/datascienceadvice/multichainr/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/multichainr)](https://CRAN.R-project.org/package=multichainr)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**multichainr** is a comprehensive R interface to the [MultiChain](https://www.multichain.com/) blockchain JSON-RPC API. It allows you to manage blockchain nodes, create and query data streams, issue assets, manage permissions, build raw transactions, and much more -- all from within R.

---

## Prerequisites

- **R** >= 3.6.0
- **MultiChain** >= 2.0 -- download from [multichain.com/download-install](https://www.multichain.com/download-install/)
- **Pandoc** (only if building vignettes locally)

Make sure the MultiChain binaries (`multichaind`, `multichain-util`) are accessible. Set the path via:

```r
mc_set_path("/path/to/multichain/binaries")
```

or add `MULTICHAIN_PATH` to your `.Renviron` file.

---

## Installation

```r
# From GitHub (development version)
remotes::install_github("datascienceadvice/multichainr")

# From CRAN (once published)
install.packages("multichainr")
```

---

## Quick Start

```r
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

---

## Core Features

| Area | Key Functions |
|------|---------------|
| **Connection** | `mc_connect()`, `mc_get_config()` |
| **Node Management** | `mc_node_init()`, `mc_node_start()`, `mc_node_stop()` |
| **Addresses** | `mc_get_new_address()`, `mc_get_addresses()`, `mc_validate_address()` |
| **Assets** | `mc_issue()`, `mc_issue_from()`, `mc_list_assets()`, `mc_get_asset_info()` |
| **Streams** | `mc_create_stream()`, `mc_publish()`, `mc_list_stream_items()` |
| **Permissions** | `mc_grant()`, `mc_revoke()`, `mc_list_permissions()` |
| **Payments** | `mc_send()`, `mc_send_asset()`, `mc_send_with_data()` |
| **Raw Transactions** | `mc_create_raw_transaction()`, `mc_sign_raw_transaction()`, `mc_decode_raw_transaction()` |
| **Atomic Exchange** | `mc_create_raw_exchange()`, `mc_append_raw_exchange()`, `mc_complete_raw_exchange()` |
| **Smart Filters** | `mc_create_stream_filter()`, `mc_create_tx_filter()`, `mc_test_tx_filter()` |
| **Libraries & Variables** | `mc_create_library()`, `mc_create_variable()`, `mc_set_variable_value()` |
| **Subscriptions** | `mc_subscribe()`, `mc_unsubscribe()` |
| **Binary Cache** | `mc_create_binary_cache()`, `mc_txout_to_binary_cache()` |
| **Network** | `mc_add_node()`, `mc_get_peer_info()`, `mc_ping()` |
| **Messaging** | `mc_sign_message()`, `mc_verify_message()` |
| **Wallet** | `mc_backup_wallet()`, `mc_encrypt_wallet()`, `mc_dump_privkey()` |

---

## Documentation

Full reference documentation is available at the [package website](https://datascienceadvice.github.io/multichainr/) (once published) or via R's built-in help:

```r
?mc_connect    # help for a specific function
help(package = "multichainr")  # list all exported functions
```

---

## Contributing

Contributions are welcome! Please:

1. Open an [issue](https://github.com/datascienceadvice/multichainr/issues) to report bugs or suggest features.
2. Submit a pull request with a clear description of the change.
3. Ensure `devtools::check()` passes before submitting.

---

## License

This project is licensed under the MIT License -- see the [LICENSE](LICENSE) file for details.
