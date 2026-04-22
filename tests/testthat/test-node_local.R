test_that("mc_set_path stops if directory does not exist", {
  expect_error(mc_set_path(tempfile()), "Directory does not exist.")
})

test_that("mc_set_path sets option correctly", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  
  mc_set_path(tmp_dir)
  expect_equal(normalizePath(getOption("multichainr.path")), normalizePath(tmp_dir))
})

test_that("mc_get_bin_path finds binary in set path", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  
  fake_bin <- file.path(tmp_dir, "multichaind")
  if (.Platform$OS.type == "windows") fake_bin <- paste0(fake_bin, ".exe")
  writeLines("fake", fake_bin)
  
  withr::local_envvar(PATH = paste(tmp_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  
  result <- mc_get_bin_path("multichaind")
  expect_equal(normalizePath(result), normalizePath(fake_bin))
})

test_that("mc_get_bin_path finds binary in system PATH", {
  options(multichainr.path = NULL)
  
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  
  fake_bin <- file.path(tmp_dir, "multichaind")
  if (.Platform$OS.type == "windows") fake_bin <- paste0(fake_bin, ".exe")
  writeLines("fake", fake_bin)
  
  withr::local_envvar(PATH = paste(tmp_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  
  result <- mc_get_bin_path("multichaind")
  expect_equal(normalizePath(result), normalizePath(fake_bin))
})

test_that("mc_get_bin_path throws error if not found", {
  withr::local_options(list(multichainr.path = NULL))
  withr::local_envvar(list(MULTICHAINR_PATH = "")) 
  
  bin_name <- "this_program_does_not_exist_xyz"
  
  expect_error(
    mc_get_bin_path(bin_name),
    regexp = "not found"
  )
})

test_that("mc_get_config correctly parses configuration files", {
  
  tmp_base <- tempfile("multichain_test_") 
  dir.create(tmp_base)
  
  chain_name <- "test_chain_xyz"
  chain_dir <- file.path(tmp_base, chain_name)
  dir.create(chain_dir)
  
  conf_content <- c(
    "rpcuser=test_user_name",
    "rpcpassword=test_password_123"
  )
  writeLines(conf_content, file.path(chain_dir, "multichain.conf"))
  
  params_content <- c(
    "default-rpc-port=7788"
  )
  writeLines(params_content, file.path(chain_dir, "params.dat"))
  
  config <- mc_get_config(chain_name, base_dir = tmp_base)
  
  expect_type(config, "list")
  expect_equal(config$user, "test_user_name")
  expect_equal(config$password, "test_password_123")
  expect_equal(config$port, 7788)
  
  cat("\nrpcport=9999\n", 
      file = file.path(chain_dir, "multichain.conf"), 
      append = TRUE)
  
  config_updated <- mc_get_config(chain_name, base_dir = tmp_base)
  expect_equal(config_updated$port, 9999)
  
  unlink(tmp_base, recursive = TRUE)
})

test_that("mc_get_config throws error if directory does not exist", {
  expect_error(mc_get_config("non_existent_chain", base_dir = "/tmp/fake_path"))
})

test_that("mc_get_config uses Windows default base_dir", {
  skip_if_not(.Platform$OS.type == "windows")
  
  original_appdata <- Sys.getenv("APPDATA")
  tmp_appdata <- tempfile()
  dir.create(tmp_appdata)
  withr::local_envvar(APPDATA = tmp_appdata)
  on.exit(unlink(tmp_appdata, recursive = TRUE), add = TRUE)
  
  chain_dir <- file.path(tmp_appdata, "MultiChain", "testchain")
  dir.create(chain_dir, recursive = TRUE)
  writeLines("rpcuser=winuser", file.path(chain_dir, "multichain.conf"))
  writeLines("default-rpc-port=9999", file.path(chain_dir, "params.dat"))
  
  config <- mc_get_config("testchain")
  expect_equal(config$user, "winuser")
  expect_equal(config$port, 9999)
})

test_that("mc_get_config uses macOS default base_dir", {
  skip_if_not(Sys.info()["sysname"] == "Darwin")
  
  home <- Sys.getenv("HOME")
  tmp_home <- tempfile()
  dir.create(tmp_home)
  withr::local_envvar(HOME = tmp_home)
  on.exit(unlink(tmp_home, recursive = TRUE), add = TRUE)
  
  chain_dir <- file.path(tmp_home, "Library/Application Support/MultiChain", "testchain")
  dir.create(chain_dir, recursive = TRUE)
  writeLines("rpcuser=macuser", file.path(chain_dir, "multichain.conf"))
  writeLines("default-rpc-port=7777", file.path(chain_dir, "params.dat"))
  
  config <- mc_get_config("testchain")
  expect_equal(config$user, "macuser")
  expect_equal(config$port, 7777)
})

test_that("mc_get_config uses Linux default base_dir", {
  skip_if_not(.Platform$OS.type == "unix" && Sys.info()["sysname"] != "Darwin")
  
  home <- Sys.getenv("HOME")
  tmp_home <- tempfile()
  dir.create(tmp_home)
  withr::local_envvar(HOME = tmp_home)
  on.exit(unlink(tmp_home, recursive = TRUE), add = TRUE)
  
  chain_dir <- file.path(tmp_home, ".multichain", "testchain")
  dir.create(chain_dir, recursive = TRUE)
  writeLines("rpcuser=linuxuser", file.path(chain_dir, "multichain.conf"))
  writeLines("default-rpc-port=5555", file.path(chain_dir, "params.dat"))
  
  config <- mc_get_config("testchain")
  expect_equal(config$user, "linuxuser")
  expect_equal(config$port, 5555)
})

skip_on_cran()


cleanup_chain <- function(chain_name) {
  try(mc_node_stop(chain_name), silent = TRUE)
  
  if (.Platform$OS.type == "windows") {
    base_dir <- file.path(Sys.getenv("APPDATA"), "MultiChain")
  } else if (Sys.info()["sysname"] == "Darwin") {
    base_dir <- file.path(Sys.getenv("HOME"), "Library/Application Support/MultiChain")
  } else {
    base_dir <- file.path(Sys.getenv("HOME"), ".multichain")
  }
  chain_dir <- file.path(base_dir, chain_name)
  if (dir.exists(chain_dir)) {
    unlink(chain_dir, recursive = TRUE)
  }
}

test_that("Integration: create, start, stop chain using set path", {
  current_path <- getOption("multichainr.path")
  
  if (is.null(current_path)) {
    env_path <- Sys.getenv("MULTICHAIN_PATH")
    if (env_path != "" && dir.exists(env_path)) {
      mc_set_path(env_path)
      current_path <- getOption("multichainr.path")
    }
  }
  
  skip_if(is.null(current_path), "MULTICHAIN_PATH not set. Integration tests skipped.")
  
  chain_name <- paste0("test_integration_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  chain_name <- gsub("[^a-zA-Z0-9]", "_", chain_name)
  
  withr::defer(cleanup_chain(chain_name))
  
  expect_message(mc_node_init(chain_name), "Blockchain created")
  
  expect_message(mc_node_start(chain_name), "Start command issued.")
  Sys.sleep(3)
  
  expect_message(mc_node_stop(chain_name), "Stop signal sent.")
  Sys.sleep(2)
  
  expect_message(mc_node_start(chain_name), "Start command issued.")
  Sys.sleep(2)
  
  config <- mc_get_config(chain_name)
  conn <- mc_connect(config)
  expect_message(mc_node_stop(conn), "Stop signal sent.")
})