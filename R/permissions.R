#' Grant permissions to an address
#'
#' Grants one or more permissions to a wallet address. Permissions control what
#' actions an address can perform on the blockchain (e.g., connect, send, receive,
#' mine, admin, etc.).
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param address Character string. The address that will receive the permissions.
#' @param permissions Character string. A comma‑separated list of permissions
#'   to grant, e.g., \code{"connect,send,receive"}.
#'
#' @return A character string containing the transaction ID (txid) of the grant.
#'
#' @examples
#' \dontrun{
#' # Grant connect and send permissions to an address
#' txid <- mc_grant(conn, "1A...", "connect,send")
#' }
#'
#' @seealso \code{\link{mc_grant_from}} to specify the grantor,
#'   \code{\link{mc_revoke}} to revoke permissions,
#'   \code{\link{mc_list_permissions}} to view permissions.
#'
#' @family permissions
#' @export
mc_grant <- function(conn, address, permissions) {
  mc_rpc(conn, "grant", list(address, permissions))
}

#' Grant permissions from a specific address
#'
#' Grants permissions from a specified address (which must have admin or
#' grant rights). This allows controlling which address pays for the
#' transaction and acts as the grantor.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. The address granting the permissions.
#' @param to_address Character string. The address receiving the permissions.
#' @param permissions Character string. Comma‑separated list of permissions.
#' @param native_amount Numeric. Amount of native currency to send along with
#'   the grant (default 0).
#' @param start_block Optional integer. Block height from which the permission
#'   becomes valid. If \code{NULL}, the permission starts immediately.
#' @param end_block Optional integer. Block height at which the permission expires.
#'   If \code{NULL}, the permission never expires.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Grant permissions from a specific admin address
#' txid <- mc_grant_from(conn, "admin1...", "user1...", "send,receive")
#'
#' # Grant with a validity window
#' txid <- mc_grant_from(conn, "admin1...", "user1...", "mine",
#'                       start_block = 1000, end_block = 2000)
#' }
#'
#' @seealso \code{\link{mc_grant}}, \code{\link{mc_revoke_from}}.
#'
#' @family permissions
#' @export
mc_grant_from <- function(conn, from_address, to_address, permissions, 
                          native_amount = 0, start_block = NULL, end_block = NULL) {
  params <- list(from_address, to_address, permissions, native_amount)
  if (!is.null(start_block)) params <- c(params, list(start_block))
  if (!is.null(end_block)) params <- c(params, list(end_block))
  
  mc_rpc(conn, "grantfrom", params)
}

#' Grant permissions with metadata
#'
#' Grants permissions and attaches arbitrary data (metadata) to the transaction.
#' The data can be text, JSON, or any hex‑encoded value.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param to_address Character string. The address receiving the permissions.
#' @param permissions Character string. Comma‑separated list of permissions.
#' @param data Data to embed. Can be a character string (will be hex‑encoded),
#'   a list (converted to JSON then hex), or raw binary.
#' @param native_amount Numeric. Amount of native currency to send (default 0).
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Grant with a text note
#' txid <- mc_grant_with_data(conn, "1A...", "send",
#'                            data = "Welcome to the network!")
#'
#' # Grant with JSON metadata
#' metadata <- list(reason = "partnership", level = "full")
#' txid <- mc_grant_with_data(conn, "1A...", "connect,send",
#'                            data = metadata, native_amount = 0.1)
#' }
#'
#' @seealso \code{\link{mc_grant_with_data_from}} to specify the grantor.
#'
#' @family permissions
#' @export
mc_grant_with_data <- function(conn, to_address, permissions, data, native_amount = 0) {
  if (is.list(data)) {
    data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  }
  data_hex <- paste(charToRaw(as.character(data)), collapse = "")
  
  params <- list(to_address, permissions, data_hex, native_amount)
  mc_rpc(conn, "grantwithdata", params)
}

#' Grant permissions from a specific address with metadata
#'
#' Grants permissions from a specified address and includes metadata.
#' Combines the capabilities of \code{\link{mc_grant_from}} and
#' \code{\link{mc_grant_with_data}}.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. The address granting the permissions.
#' @param to_address Character string. The address receiving the permissions.
#' @param permissions Character string. Comma‑separated list of permissions.
#' @param data Data to embed (string or list, automatically hex‑encoded).
#' @param native_amount Numeric. Amount of native currency to send (default 0).
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Grant from a specific admin with metadata
#' txid <- mc_grant_with_data_from(conn, "admin1...", "user1...",
#'                                 "send,receive",
#'                                 data = list(note = "temporary access"),
#'                                 native_amount = 0.01)
#' }
#'
#' @seealso \code{\link{mc_grant_with_data}}, \code{\link{mc_grant_from}}.
#'
#' @family permissions
#' @export
mc_grant_with_data_from <- function(conn, from_address, to_address, permissions, data, native_amount = 0) {
  if (is.list(data)) {
    data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  }
  data_hex <- paste(charToRaw(as.character(data)), collapse = "")
  
  params <- list(from_address, to_address, permissions, data_hex, native_amount)
  mc_rpc(conn, "grantwithdatafrom", params)
}

#' List network permissions
#'
#' Returns a list of all permissions (or filtered by type) currently active
#' on the blockchain.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param permissions Character string. Permission type to filter.
#'   Can be a single permission name (e.g., \code{"send"}) or \code{"*"} (default)
#'   to list all permissions.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with permission entries,
#'   typically containing columns like \code{address}, \code{type}, \code{start},
#'   \code{end}, and \code{txid}.
#'
#' @examples
#' \dontrun{
#' # List all permissions
#' all_perms <- mc_list_permissions(conn)
#'
#' # List only "admin" permissions
#' admins <- mc_list_permissions(conn, "admin")
#' }
#'
#' @seealso \code{\link{mc_grant}}, \code{\link{mc_revoke}}.
#'
#' @family permissions
#' @export
mc_list_permissions <- function(conn, permissions = "*") {
  res <- mc_rpc(conn, "listpermissions", list(permissions))
  rpc_res_to_df(res)
}

#' Revoke permissions from an address
#'
#' Removes one or more permissions from a wallet address. The revoking address
#' must have admin rights.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param address Character string. The address from which permissions are revoked.
#' @param permissions Character string. Comma‑separated list of permissions to revoke.
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Revoke send and receive permissions
#' txid <- mc_revoke(conn, "1A...", "send,receive")
#' }
#'
#' @seealso \code{\link{mc_revoke_from}} to specify the revoker,
#'   \code{\link{mc_grant}} for granting.
#'
#' @family permissions
#' @export
mc_revoke <- function(conn, address, permissions) {
  mc_rpc(conn, "revoke", list(address, permissions))
}

#' Revoke permissions from a specific address
#'
#' Revokes permissions from an address, specifying the address that issues the
#' revocation. This allows controlling which address pays for the transaction.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. The address revoking the permissions.
#' @param to_address Character string. The address losing the permissions.
#' @param permissions Character string. Comma‑separated list of permissions to revoke.
#' @param native_amount Numeric. Amount of native currency to send along with
#'   the revocation (default 0).
#'
#' @return A character string containing the transaction ID.
#'
#' @examples
#' \dontrun{
#' # Revoke from a specific admin address
#' txid <- mc_revoke_from(conn, "admin1...", "user1...", "send")
#' }
#'
#' @seealso \code{\link{mc_revoke}}, \code{\link{mc_grant_from}}.
#'
#' @family permissions
#' @export
mc_revoke_from <- function(conn, from_address, to_address, permissions, native_amount = 0) {
  params <- list(from_address, to_address, permissions, native_amount)
  mc_rpc(conn, "revokefrom", params)
}

#' Verify if an address has a specific permission
#'
#' Checks whether a given address has a particular permission on the blockchain.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param address Character string. The address to check.
#' @param permission Character string. The permission name (e.g., \code{"send"},
#'   \code{"admin"}, \code{"connect"}).
#'
#' @return A logical value: \code{TRUE} if the address has the permission,
#'   \code{FALSE} otherwise.
#'
#' @examples
#' \dontrun{
#' # Check if an address can send
#' can_send <- mc_verify_permission(conn, "1A...", "send")
#' if (can_send) cat("Address can send assets")
#' }
#'
#' @seealso \code{\link{mc_list_permissions}} to see all permissions.
#'
#' @family permissions
#' @export
mc_verify_permission <- function(conn, address, permission) {
  res <- mc_rpc(conn, "verifypermission", list(address, permission))
  return(as.logical(res))
}
