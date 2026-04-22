skip_on_cran()

bin_dir <- Sys.getenv("MULTICHAIN_PATH")
has_multichain <- bin_dir != "" && dir.exists(bin_dir)

test_that("Integration: Path Management and Node lifecycle", {
  
  # 1. mc_set_path -------------------------------------------------------------
  # Create a temporary directory to simulate a binary folder
  mock_bin_dir <- tempfile("mock_bin_")
  dir.create(mock_bin_dir)
  on.exit(unlink(mock_bin_dir, recursive = TRUE), add = TRUE)
  
  # Create a dummy file to act as the executable
  exe_name <- if (.Platform$OS.type == "windows") "multichaind.exe" else "multichaind"
  file.create(file.path(mock_bin_dir, exe_name))
  
  # Test setting the path
  mc_set_path(mock_bin_dir)
  expect_equal(normalizePath(getOption("multichain.path")), normalizePath(mock_bin_dir))
  
  # 2. mc_get_bin_path ---------------------------------------------------------
  # Test finding the binary in the custom path
  found_path <- mc_get_bin_path("multichaind")
  expect_true(grepl("multichaind", found_path))
  expect_true(file.exists(found_path))
  
  # Test error for non-existent directory
  expect_error(mc_set_path("/non/existent/path/at/all"), "Directory does not exist")
  
  # 3. mc_get_config -----------------------------------------------------------
  # Setup a mock data directory structure
  mock_data_dir <- tempfile("mock_data_")
  chain_name <- "test_config_chain"
  chain_dir <- file.path(mock_data_dir, chain_name)
  dir.create(chain_dir, recursive = TRUE)
  on.exit(unlink(mock_data_dir, recursive = TRUE), add = TRUE)
  
  # Create a dummy multichain.conf
  conf_lines <- c(
    "rpcuser=testuser",
    "rpcpassword=testpass",
    "rpcport=8888"
  )
  writeLines(conf_lines, file.path(chain_dir, "multichain.conf"))
  
  # Test reading the configuration
  config <- mc_get_config(chain_name, base_dir = mock_data_dir)
  
  expect_type(config, "list")
  expect_equal(config$user, "testuser")
  expect_equal(config$password, "testpass")
  expect_equal(config$port, 8888)
  expect_equal(config$host, "127.0.0.1")
  
  # Test fallback to params.dat for port if not in .conf
  writeLines("rpcuser=u\nrpcpassword=p", file.path(chain_dir, "multichain.conf"))
  writeLines("default-rpc-port = 9999", file.path(chain_dir, "params.dat"))
  
  config_fallback <- mc_get_config(chain_name, base_dir = mock_data_dir)
  expect_equal(config_fallback$port, 9999)
  
  # Real Node Lifecycle (Init -> Start -> Stop)
  # This section requires an actual MultiChain installation
  
  if (!has_multichain) {
    skip("MULTICHAIN_PATH not set. Skipping real node lifecycle tests.")
  }
  
  mc_set_path(bin_dir)
  real_chain_name <- paste0("lifecycle_test_", round(as.numeric(Sys.time())))
  
  # Determine real data dir for manual cleanup
  if (.Platform$OS.type == "windows") {
    real_base <- file.path(Sys.getenv("APPDATA"), "MultiChain")
  } else if (Sys.info()["sysname"] == "Darwin") {
    real_base <- file.path(Sys.getenv("HOME"), "Library/Application Support/MultiChain")
  } else {
    real_base <- file.path(Sys.getenv("HOME"), ".multichain")
  }
  real_chain_dir <- file.path(real_base, real_chain_name)
  
  # 4. mc_node_init ------------------------------------------------------------
  expect_message(mc_node_init(real_chain_name), "Blockchain created")
  expect_true(dir.exists(real_chain_dir))
  
  # 5. mc_node_start -----------------------------------------------------------
  expect_message(mc_node_start(real_chain_name), "Start command issued")
  Sys.sleep(5) # Wait for node to bind to the port
  
  # 6. mc_node_stop ------------------------------------------------------------
  # This tests the internal logic of mc_node_stop where it 
  # calls mc_get_config -> mc_connect -> RPC stop
  expect_message(mc_node_stop(real_chain_name), "Stop signal sent")
  
  # Final Cleanup of the real created directory
  Sys.sleep(2)
  if (dir.exists(real_chain_dir)) unlink(real_chain_dir, recursive = TRUE)

})
