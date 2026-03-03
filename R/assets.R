#' List blockchain assets
#' @param conn Connection object.
#' @export
mc_list_assets <- function(conn) {
  res <- mc_rpc(conn, "listassets")
  rpc_res_to_df(res)
}

#' Issue new asset
#' 
#' @param conn Connection object.
#' @param address Wallet address to receive the issued assets.
#' @param name Asset name string or list of parameters.
#' @param quantity Total amount to issue.
#' @param units Smallest divisible unit (e.g., 0.01).
#' @export
mc_issue <- function(conn, address, name, quantity, units = 1) {
  asset_params <- if (is.list(name)) name else list(name = name)
  params <- list(address, asset_params, as.numeric(quantity), as.numeric(units))
  mc_rpc(conn, "issue", params)
}

#' Send asset to another address
#' 
#' @param conn Connection object.
#' @param address Recipient address.
#' @param asset Asset name or assetref.
#' @param quantity Amount to send.
#' @export
mc_send_asset <- function(conn, address, asset, quantity) {
  params <- list(address, asset, as.numeric(quantity))
  mc_rpc(conn, "sendasset", params)
}

#' Get asset balances for an address
#' 
#' @param conn Connection object.
#' @param address Wallet address.
#' @export
mc_get_address_balances <- function(conn, address) {
  res <- mc_rpc(conn, "getaddressbalances", list(address))
  rpc_res_to_df(res)
}