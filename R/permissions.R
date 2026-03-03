#' List network permissions
#' 
#' @param conn A multichain_conn object.
#' @param permissions Permission type to filter (default "*").
#' @export
mc_list_permissions <- function(conn, permissions = "*") {
  res <- mc_rpc(conn, "listpermissions", list(permissions))
  rpc_res_to_df(res)
}

#' Grant permissions to an address
#' 
#' @param conn A multichain_conn object.
#' @param address Target wallet address.
#' @param permissions Permission string (e.g., "connect,send,receive").
#' @export
mc_grant <- function(conn, address, permissions) {
  mc_rpc(conn, "grant", list(address, permissions))
}

#' Revoke permissions from an address
#' 
#' @param conn A multichain_conn object.
#' @param address Target wallet address.
#' @param permissions Permission string to revoke.
#' @export
mc_revoke <- function(conn, address, permissions) {
  mc_rpc(conn, "revoke", list(address, permissions))
}