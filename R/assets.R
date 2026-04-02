#' Get information about a specific asset
#'
#' Retrieves details about an asset on the MultiChain blockchain. The asset can
#' be identified by its name, reference, or issuance transaction ID.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param asset Character string. Asset name, reference, or issuance transaction ID.
#' @param verbose Logical. If \code{TRUE}, returns details about individual
#'   issuances (for fungible assets with multiple issuances). Default is \code{FALSE}.
#'
#' @return A list (or data frame, depending on verbosity) containing asset
#'   information such as name, type, total quantity, units, etc.
#'
#' @examples
#' \dontrun{
#' # Get basic info about an asset named "mycoin"
#' info <- mc_get_asset_info(conn, "mycoin")
#' print(info$name)
#'
#' # Get verbose info with issuance details
#' info_verbose <- mc_get_asset_info(conn, "mycoin", verbose = TRUE)
#' }
#'
#' @seealso \code{\link{mc_list_assets}} to list all assets,
#'   \code{\link{mc_list_asset_issues}} to list issuances.
#'
#' @family assets
#' @export
mc_get_asset_info <- function(conn, asset, verbose = FALSE) {
  mc_rpc(conn, "getassetinfo", list(asset, verbose))
}

#' Get information about a specific token (MultiChain 2.2.1+)
#'
#' Retrieves details about a token (non‑fungible) within a parent asset.
#' This function requires MultiChain version 2.2.1 or later.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param asset Character string. Parent asset name, reference, or issuance
#'   transaction ID.
#' @param token Character string. Token name or index (e.g., \code{"token0"}).
#' @param verbose Logical. If \code{TRUE}, returns detailed information,
#'   including token details. Default is \code{FALSE}.
#'
#' @return A list with token information, such as name, quantity, and
#'   (if \code{verbose}) custom fields.
#'
#' @examples
#' \dontrun{
#' # Get basic info about token "nft1" in asset "art"
#' token_info <- mc_get_token_info(conn, "art", "nft1")
#' }
#'
#' @seealso \code{\link{mc_issue_token}} to issue tokens,
#'   \code{\link{mc_list_assets}} to list parent assets.
#'
#' @family assets
#' @export
mc_get_token_info <- function(conn, asset, token, verbose = FALSE) {
  mc_rpc(conn, "gettokeninfo", list(asset, token, verbose))
}

#' Issue new asset
#'
#' Creates a new asset on the MultiChain blockchain. The asset can be
#' fungible or non‑fungible, and its properties (e.g., open/restricted,
#' divisible) are specified via the \code{name} parameter.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param address Character string. Wallet address that will receive the
#'   issued assets.
#' @param name Either a character string (asset name) or a list of asset
#'   parameters (e.g., \code{list(name = "myasset", open = TRUE)}). See
#'   MultiChain documentation for supported parameters.
#' @param quantity Numeric. Total amount to issue. For non‑fungible assets,
#'   this is typically \code{1}.
#' @param units Numeric. The smallest divisible unit (e.g., \code{0.01} means
#'   the asset is divisible to two decimal places). Default is \code{1}
#'   (indivisible).
#' @param native_amount Numeric (optional). Amount of native currency (coins)
#'   to send together with the asset issuance.
#' @param custom_fields List (optional). Custom fields to attach to the asset.
#'
#' @return A character string containing the transaction ID of the issuance.
#'
#' @examples
#' \dontrun{
#' # Issue a simple fungible asset
#' txid <- mc_issue(conn, "1A...", "mycoin", quantity = 1000, units = 0.01)
#'
#' # Issue a restricted, non‑fungible asset with custom fields
#' params <- list(name = "artwork", open = FALSE, restrict = TRUE)
#' txid <- mc_issue(conn, "1A...", params, quantity = 1, units = 1,
#'                  custom_fields = list(author = "Picasso"))
#' }
#'
#' @seealso \code{\link{mc_issue_from}} to issue from a specific address,
#'   \code{\link{mc_issue_more}} to increase supply of a fungible asset.
#'
#' @family assets
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
#' Issues a new asset, but allows specifying the sender address (which must
#' have sufficient native currency to pay for the transaction). This is useful
#' when the node has multiple addresses.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. Address that will pay for and issue
#'   the asset.
#' @param to_address Character string. Recipient address (can be the same as
#'   \code{from_address}).
#' @param name Either a character string (asset name) or a list of asset
#'   parameters (e.g., \code{list(name = "myasset", open = TRUE)}).
#' @param quantity Numeric. Total amount to issue.
#' @param units Numeric. Smallest divisible unit. Default is \code{1}.
#' @param native_amount Numeric (optional). Amount of native currency to send
#'   along with the issuance.
#' @param custom_fields List (optional). Custom fields.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Issue from a specific address to the same address
#' txid <- mc_issue_from(conn, from_address = "1A...", to_address = "1A...",
#'                       name = "myasset", quantity = 100)
#' }
#'
#' @seealso \code{\link{mc_issue}} for simpler issuance,
#'   \code{\link{mc_issue_more_from}} to increase supply.
#'
#' @family assets
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
#' Increases the supply of a previously issued fungible asset. The asset must
#' be open for further issuances (i.e., its \code{open} property must be
#' \code{TRUE}).
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param address Character string. Address that will receive the newly issued
#'   units.
#' @param asset Character string. Asset name, reference, or issuance transaction ID.
#' @param quantity Numeric. Additional quantity to issue.
#' @param native_amount Numeric (optional). Amount of native currency to send.
#' @param custom_fields List (optional). Custom fields (overwrites existing ones
#'   if present).
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Increase supply of "mycoin" by 500 units
#' txid <- mc_issue_more(conn, "1A...", "mycoin", quantity = 500)
#' }
#'
#' @seealso \code{\link{mc_issue}} for initial issuance,
#'   \code{\link{mc_issue_more_from}} to issue from a specific address.
#'
#' @family assets
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
#'
#' Increases the supply of a fungible asset, specifying the address that pays
#' for and initiates the transaction.
#'
#' @inheritParams mc_issue_more
#' @param from_address Character string. Address that will pay for and issue
#'   the additional units.
#' @param to_address Character string. Recipient address.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Issue more from a specific address to another address
#' txid <- mc_issue_more_from(conn, "1A...", "1B...", "mycoin", quantity = 100)
#' }
#'
#' @seealso \code{\link{mc_issue_more}} for simpler usage,
#'   \code{\link{mc_issue_from}} for initial issuance.
#'
#' @family assets
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
#' Creates one or more tokens (non‑fungible items) under a parent asset.
#' The parent asset must be non‑fungible (e.g., issued with \code{type = "nonfungible"}).
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param address Character string. Address that will receive the tokens.
#' @param asset Character string. Parent asset name, reference, or issuance ID.
#' @param token Character string. Token name (must be unique within the asset).
#' @param quantity Numeric. Number of token units to issue (usually \code{1}).
#' @param native_amount Numeric (optional). Amount of native currency to send.
#' @param token_details List (optional). Custom details for the token
#'   (e.g., \code{list(description = "My NFT")}).
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Issue a token "painting1" under asset "art"
#' txid <- mc_issue_token(conn, "1A...", "art", "painting1", quantity = 1,
#'                        token_details = list(artist = "Monet", year = 2025))
#' }
#'
#' @seealso \code{\link{mc_get_token_info}} to query token details,
#'   \code{\link{mc_issue_token_from}} for using a specific sender address.
#'
#' @family assets
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
#'
#' Issues tokens (non‑fungible) and specifies the sending address.
#'
#' @inheritParams mc_issue_token
#' @param from_address Character string. Address that pays for and issues the token.
#' @param to_address Character string. Recipient address.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Issue token from a specific address
#' txid <- mc_issue_token_from(conn, "1A...", "1B...", "art", "painting2",
#'                             quantity = 1, token_details = list(artist = "Picasso"))
#' }
#'
#' @seealso \code{\link{mc_issue_token}} for simpler usage.
#'
#' @family assets
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
#' Returns a list of all issuance transactions (initial and subsequent)
#' for a given asset.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param asset Character string. Asset name, reference, or issuance ID.
#' @param verbose Logical. If \code{TRUE}, includes details about issuers
#'   and custom fields. Default is \code{FALSE}.
#' @param count Integer (optional). Maximum number of issuances to return.
#' @param start Integer (optional). Offset (positive for forward, negative
#'   for backward from the most recent). Use a negative value to get the
#'   most recent issuances first.
#'
#' @return A data frame (converted via \code{rpc_res_to_df}) with one row per
#'   issuance. Columns typically include \code{txid}, \code{issuer}, \code{quantity},
#'   \code{units}, etc.
#'
#' @examples
#' \dontrun{
#' # Get all issuances of "mycoin"
#' issues <- mc_list_asset_issues(conn, "mycoin")
#'
#' # Get the most recent 5 issuances with details
#' recent <- mc_list_asset_issues(conn, "mycoin", verbose = TRUE,
#'                                count = 5, start = -5)
#' }
#'
#' @seealso \code{\link{mc_list_assets}} to list assets,
#'   \code{\link{mc_get_asset_info}} for asset summary.
#'
#' @family assets
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
#' Returns a list of assets on the blockchain, with optional filtering
#' and pagination.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param assets Asset filter. Can be a single asset name/ref/txid, a vector
#'   of such identifiers, or \code{"*"} (default) to list all assets.
#' @param verbose Logical. If \code{TRUE}, returns detailed information
#'   (e.g., issuances, open status). Default is \code{FALSE}.
#' @param count Integer (optional). Maximum number of assets to return.
#' @param start Integer (optional). Offset for pagination.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with asset information.
#'   If \code{verbose = FALSE}, columns include \code{name}, \code{ref},
#'   \code{issuetxid}, etc. If \code{verbose = TRUE}, additional details
#'   like issuances and open status are included.
#'
#' @examples
#' \dontrun{
#' # List all assets
#' all_assets <- mc_list_assets(conn)
#'
#' # Get details for a specific asset
#' asset_detail <- mc_list_assets(conn, assets = "mycoin", verbose = TRUE)
#'
#' # List first 10 assets
#' first_10 <- mc_list_assets(conn, count = 10)
#' }
#'
#' @seealso \code{\link{mc_get_asset_info}} for single asset details.
#'
#' @family assets
#' @export
mc_list_assets <- function(conn, assets = "*", verbose = FALSE, count = NULL, start = NULL) {
  params <- list(assets, verbose)
  if (!is.null(count)) params <- c(params, list(as.integer(count)))
  if (!is.null(start)) {
    if (is.null(count)) params <- c(params, list(2147483647)) # MAX_INT
    params <- c(params, list(as.integer(start)))
  }
  res <- mc_rpc(conn, "listassets", params)
  rpc_res_to_df(res)
}

#' Update asset status (open/closed)
#'
#' Changes the status of an asset (e.g., to open or close further issuances).
#' The asset must have been created with the ability to be updated.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param asset Character string. Asset name, reference, or issuance ID.
#' @param params A list of parameters to update, typically
#'   \code{list(open = FALSE)} to close further issuances.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Close the asset "mycoin" to prevent further issuances
#' txid <- mc_update(conn, "mycoin", list(open = FALSE))
#' }
#'
#' @seealso \code{\link{mc_update_from}} to update from a specific address.
#'
#' @family assets
#' @export
mc_update <- function(conn, asset, params) {
  mc_rpc(conn, "update", list(asset, params))
}

#' Update asset status from specific address
#'
#' Changes the status of an asset (e.g., open/close) and specifies the address
#' that pays for and authorizes the update.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. Address that pays for the transaction.
#' @param asset Character string. Asset name, reference, or issuance ID.
#' @param params A list of parameters to update.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Close the asset from a specific address
#' txid <- mc_update_from(conn, "1A...", "mycoin", list(open = FALSE))
#' }
#'
#' @seealso \code{\link{mc_update}} for simpler usage.
#'
#' @family assets
#' @export
mc_update_from <- function(conn, from_address, asset, params) {
  mc_rpc(conn, "updatefrom", list(from_address, asset, params))
}

#' Get a specific transaction involving a subscribed asset
#'
#' Returns details of a single transaction that affects a subscribed asset.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param asset Character string. Subscribed asset name, reference, or issuance ID.
#' @param txid Character string. Transaction ID.
#' @param verbose Logical. If \code{TRUE}, includes additional details.
#'
#' @return A list (or data frame) with transaction details, including inputs,
#'   outputs, and asset movements.
#'
#' @examples
#' \dontrun{
#' # Get details of a specific transaction
#' tx <- mc_get_asset_transaction(conn, "mycoin", "txid...")
#' }
#'
#' @seealso \code{\link{mc_list_asset_transactions}} to list multiple transactions.
#'
#' @family asset transactions
#' @export
mc_get_asset_transaction <- function(conn, asset, txid, verbose = FALSE) {
  mc_rpc(conn, "getassettransaction", list(asset, txid, verbose))
}

#' List transactions involving a subscribed asset
#'
#' Returns a list of recent transactions that affect a subscribed asset.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param asset Character string. Subscribed asset name, reference, or issuance ID.
#' @param verbose Logical. If \code{TRUE}, returns detailed information
#'   about each transaction.
#' @param count Integer. Number of transactions to return (default 10).
#' @param start Integer (optional). Offset (negative for most recent).
#' @param local_ordering Logical. If \code{TRUE}, uses local node's
#'   transaction ordering (default \code{FALSE}).
#'
#' @return A data frame (via \code{rpc_res_to_df}) with transaction details.
#'
#' @examples
#' \dontrun{
#' # Get the 10 most recent transactions
#' txs <- mc_list_asset_transactions(conn, "mycoin")
#'
#' # Get next 5 transactions with details
#' more_txs <- mc_list_asset_transactions(conn, "mycoin", verbose = TRUE,
#'                                        count = 5, start = 10)
#' }
#'
#' @seealso \code{\link{mc_get_asset_transaction}} for a single transaction.
#'
#' @family asset transactions
#' @export
mc_list_asset_transactions <- function(conn, asset, verbose = FALSE, count = 10, start = NULL, local_ordering = FALSE) {
  params <- list(asset, verbose, as.integer(count))
  if (!is.null(start)) params <- c(params, list(as.integer(start)))
  else params <- c(params, list(-as.integer(count)))
  
  params <- c(params, list(local_ordering))
  res <- mc_rpc(conn, "listassettransactions", params)
  rpc_res_to_df(res)
}
