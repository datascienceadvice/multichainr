skip_on_cran()
mc_set_path(Sys.getenv("MULTICHAIN_PATH"))

bin_path <- tryCatch(mc_get_bin_path("multichaind"), error = function(e) "")
skip_if(bin_path == "", "MultiChain binaries not found. Skipping integration tests.")

test_that("Integration: Messaging and Cryptography lifecycle", {
  
  # SETUP ----------------------------------------------------------------------
  chain_name <- paste0("test_msg_", round(as.numeric(Sys.time())))
  
  # basedir
  if (.Platform$OS.type == "windows") {
    base_dir <- file.path(Sys.getenv("APPDATA"), "MultiChain")
  } else if (Sys.info()["sysname"] == "Darwin") {
    base_dir <- file.path(Sys.getenv("HOME"), "Library/Application Support/MultiChain")
  } else {
    base_dir <- file.path(Sys.getenv("HOME"), ".multichain")
  }
  chain_dir <- file.path(base_dir, chain_name)
  
  # clenaup on exit
  on.exit({
    try(mc_node_stop(chain_name), silent = TRUE)
    Sys.sleep(2)
    if (dir.exists(chain_dir)) unlink(chain_dir, recursive = TRUE)
  })
  
  # init
  mc_node_init(chain_name)
  mc_node_start(chain_name)
  
  # wait
  message("Waiting for node to initialize RPC...")
  Sys.sleep(5) 
  
  config <- mc_get_config(chain_name)
  conn <- mc_connect(config)

  # 1. mc_sign_message ---------------------------------------------------------
  # Create an address in the node's wallet
  wallet_addr <- mc_get_new_address(conn)
  test_message <- "This is a secure message from MultiChain"
  
  # Sign using the wallet address (node uses the internal private key)
  signature_1 <- mc_sign_message(conn, wallet_addr, test_message)
  
  expect_type(signature_1, "character")
  expect_true(nchar(signature_1) > 0)
  
  # 2. mc_verify_message (Success Case) ----------------------------------------
  # Verify the signature created in the previous step
  is_valid_1 <- mc_verify_message(conn, wallet_addr, signature_1, test_message)
  
  expect_type(is_valid_1, "logical")
  expect_true(is_valid_1)
  
  # Verification should fail if the message has been altered
  tampered_message <- "This is a secure message from MultiChain." # Added a dot
  is_valid_fail <- mc_verify_message(conn, wallet_addr, signature_1, tampered_message)
  
  expect_false(is_valid_fail)
  
  # Generate a keypair not stored in the wallet
  keypair <- mc_create_keypairs(conn, count = 1)
  raw_privkey <- keypair$privkey[1]
  raw_address <- keypair$address[1]
  
  ext_message <- "Message signed with raw WIF key"
  
  # Sign using the raw private key string
  signature_2 <- mc_sign_message(conn, raw_privkey, ext_message)
  
  expect_type(signature_2, "character")
  
  # Verify using the corresponding public address
  is_valid_2 <- mc_verify_message(conn, raw_address, signature_2, ext_message)
  expect_true(is_valid_2)
  
  # Verification should fail if the address does not match the key used to sign
  other_addr <- mc_get_new_address(conn)
  is_wrong_addr <- mc_verify_message(conn, other_addr, signature_2, ext_message)
  
  expect_false(is_wrong_addr)  

})
