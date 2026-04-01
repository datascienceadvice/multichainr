#' Get information about a specific asset
#' 
#' @param conn Connection object.
#' @param asset Asset name, ref or issuance txid.
#' @param verbose If TRUE, returns details about individual issuances.
#' @export
mc_get_asset_info <- function(conn, asset, verbose = FALSE) {
  mc_rpc(conn, "getassetinfo", list(asset, verbose))
}

#' Get information about a specific token (MultiChain 2.2.1+)
#' 
#' @param conn Connection object.
#' @param asset Parent asset name, ref or issuance txid.
#' @param token Token name or index.
#' @param verbose If TRUE, returns detailed information.
#' @export
mc_get_token_info <- function(conn, asset, token, verbose = FALSE) {
  mc_rpc(conn, "gettokeninfo", list(asset, token, verbose))
}

#' Issue new asset
#' 
#' @param conn Connection object.
#' @param address Wallet address to receive the issued assets.
#' @param name Asset name string or list of parameters (open, restrict, fungible, etc.).
#' @param quantity Total amount to issue.
#' @param units Smallest divisible unit (e.g., 0.01).
#' @param native_amount Optional native currency amount to send.
#' @param custom_fields Optional list of custom fields.
#' @export
mc_issue <- function(conn, address, name, quantity, units = 1, native_amount = NULL, custom_fields = NULL) {
  asset_params <- if (is.list(name)) name else list(name = name)
  params <- list(address, asset_params, as.numeric(quantity), as.numeric(units))
  
  if (!is.null(native_amount) || !is.null(custom_fields)) {
    params <- c(params, list(as.numeric(native_amount %||% 0)))
  }
  if (!is.null(custom_fields)) {
    params <- c(params, list(custom_fields))
  }
  
  mc_rpc(conn, "issue", params)
}

#' Issue new asset from specific address
#' 
#' @param conn Connection object.
#' @param from_address Address used to issue the asset.
#' @param to_address Recipient address.
#' @param name Asset name or params list.
#' @param quantity Amount to issue.
#' @param units Smallest divisible unit.
#' @param native_amount Optional native currency amount.
#' @param custom_fields Optional list of custom fields.
#' @export
mc_issue_from <- function(conn, from_address, to_address, name, quantity, units = 1, native_amount = NULL, custom_fields = NULL) {
  asset_params <- if (is.list(name)) name else list(name = name)
  
  params <- list(from_address, to_address, asset_params, as.numeric(quantity), as.numeric(units))
  
  if (!is.null(native_amount) || !is.null(custom_fields)) {
    params <- c(params, list(as.numeric(native_amount %||% 0)))
  }
  
  if (!is.null(custom_fields)) {
    params <- c(params, list(custom_fields))
  }
  
  mc_rpc(conn, "issuefrom", params)
}

#' Issue more of an existing fungible asset
#' 
#' @param conn Connection object.
#' @param address Recipient address.
#' @param asset Asset name, ref or txid.
#' @param quantity Amount to issue.
#' @param native_amount Optional native currency amount.
#' @param custom_fields Optional list of custom fields.
#' @export
mc_issue_more <- function(conn, address, asset, quantity, native_amount = NULL, custom_fields = NULL) {
  params <- list(address, asset, as.numeric(quantity))
  if (!is.null(native_amount)) params <- c(params, list(as.numeric(native_amount)))
  if (!is.null(custom_fields)) {
    if (is.null(native_amount)) params <- c(params, list(0))
    params <- c(params, list(custom_fields))
  }
  mc_rpc(conn, "issuemore", params)
}

#' Issue more of an asset from specific address
#' @export
mc_issue_more_from <- function(conn, from_address, to_address, asset, quantity, native_amount = NULL, custom_fields = NULL) {
  params <- list(from_address, to_address, asset, as.numeric(quantity))
  if (!is.null(native_amount)) params <- c(params, list(as.numeric(native_amount)))
  if (!is.null(custom_fields)) {
    if (is.null(native_amount)) params <- c(params, list(0))
    params <- c(params, list(custom_fields))
  }
  mc_rpc(conn, "issuemorefrom", params)
}

#' Issue tokens for a non-fungible asset (NFT)
#' 
#' @param conn Connection object.
#' @param address Recipient address.
#' @param asset Parent asset name, ref or txid.
#' @param token Token name.
#' @param quantity Amount of tokens.
#' @param native_amount Optional native currency amount.
#' @param token_details Optional list of token details.
#' @export
mc_issue_token <- function(conn, address, asset, token, quantity, native_amount = NULL, token_details = NULL) {
  params <- list(address, asset, token, as.numeric(quantity))
  if (!is.null(native_amount)) params <- c(params, list(as.numeric(native_amount)))
  if (!is.null(token_details)) {
    if (is.null(native_amount)) params <- c(params, list(0))
    params <- c(params, list(token_details))
  }
  mc_rpc(conn, "issuetoken", params)
}

#' Issue tokens from specific address
#' @export
mc_issue_token_from <- function(conn, from_address, to_address, asset, token, quantity, native_amount = NULL, token_details = NULL) {
  params <- list(from_address, to_address, asset, token, as.numeric(quantity))
  if (!is.null(native_amount)) params <- c(params, list(as.numeric(native_amount)))
  if (!is.null(token_details)) {
    if (is.null(native_amount)) params <- c(params, list(0))
    params <- c(params, list(token_details))
  }
  mc_rpc(conn, "issuetokenfrom", params)
}

#' List issuance events for an asset
#' 
#' @param conn Connection object.
#' @param asset Asset name, ref or txid.
#' @param verbose If TRUE, includes issuers and custom fields.
#' @param count Number of issuances to return.
#' @param start Offset (negative for most recent).
#' @export
mc_list_asset_issues <- function(conn, asset, verbose = FALSE, count = NULL, start = NULL) {
  params <- list(asset, verbose)
  if (!is.null(count)) params <- c(params, list(as.integer(count)))
  if (!is.null(start)) {
    if (is.null(count)) params <- c(params, list(2147483647)) # MAX_INT
    params <- c(params, list(as.integer(start)))
  }
  res <- mc_rpc(conn, "listassetissues", params)
  rpc_res_to_df(res)
}

#' List blockchain assets
#' 
#' @param conn Connection object.
#' @param assets Asset filter (name, ref, txid, vector of names, or "*").
#' @param verbose If TRUE, returns detailed information.
#' @param count Number of assets to return.
#' @param start Offset.
#' @export
mc_list_assets <- function(conn, assets = "*", verbose = FALSE, count = NULL, start = NULL) {
  params <- list(assets, verbose)
  if (!is.null(count)) params <- c(params, list(as.integer(count)))
  if (!is.null(start)) {
    if (is.null(count)) params <- c(params, list(2147483647))
    params <- c(params, list(as.integer(start)))
  }
  res <- mc_rpc(conn, "listassets", params)
  rpc_res_to_df(res)
}

#' Update asset status (open/closed)
#' 
#' @param conn Connection object.
#' @param asset Asset name, ref or txid.
#' @param params List, e.g., list(open = FALSE).
#' @export
mc_update <- function(conn, asset, params) {
  mc_rpc(conn, "update", list(asset, params))
}

#' Update asset status from specific address
#' @export
mc_update_from <- function(conn, from_address, asset, params) {
  mc_rpc(conn, "updatefrom", list(from_address, asset, params))
}

#' Get a specific transaction involving a subscribed asset
#' @export
mc_get_asset_transaction <- function(conn, asset, txid, verbose = FALSE) {
  mc_rpc(conn, "getassettransaction", list(asset, txid, verbose))
}

#' List transactions involving a subscribed asset
#' @export
mc_list_asset_transactions <- function(conn, asset, verbose = FALSE, count = 10, start = NULL, local_ordering = FALSE) {
  params <- list(asset, verbose, as.integer(count))
  if (!is.null(start)) params <- c(params, list(as.integer(start)))
  else params <- c(params, list(-as.integer(count)))
  
  params <- c(params, list(local_ordering))
  res <- mc_rpc(conn, "listassettransactions", params)
  rpc_res_to_df(res)
}