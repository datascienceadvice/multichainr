test_that("mc_get_config correctly parses configuration files", {
  
  # 1. Create temporary directory structure
  tmp_base <- tempfile("multichain_test_") 
  dir.create(tmp_base)
  
  chain_name <- "test_chain_xyz"
  chain_dir <- file.path(tmp_base, chain_name)
  dir.create(chain_dir)
  
  # 2. Mock multichain.conf
  conf_content <- c(
    "rpcuser=test_user_name",
    "rpcpassword=test_password_123"
  )
  writeLines(conf_content, file.path(chain_dir, "multichain.conf"))
  
  # 3. Mock params.dat
  params_content <- c(
    "default-rpc-port=7788"
  )
  writeLines(params_content, file.path(chain_dir, "params.dat"))
  
  # 4. Check base case
  config <- mc_get_config(chain_name, base_dir = tmp_base)
  
  expect_type(config, "list")
  expect_equal(config$user, "test_user_name")
  expect_equal(config$password, "test_password_123")
  expect_equal(config$port, 7788)
  
  # 5. Check rpcport priority
  cat("rpcport=9999\n", 
      file = file.path(chain_dir, "multichain.conf"), 
      append = TRUE)
  
  config_updated <- mc_get_config(chain_name, base_dir = tmp_base)
  expect_equal(config_updated$port, 9999)
  
  # Cleanup
  unlink(tmp_base, recursive = TRUE)
})

test_that("mc_get_config throws error if directory does not exist", {
  expect_error(mc_get_config("non_existent_chain", base_dir = "/tmp/fake_path"))
})