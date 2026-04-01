#' Grant permissions to an address
#' 
#' @param conn A multichain_conn object.
#' @param address Target wallet address.
#' @param permissions Permission string (e.g., "connect,send,receive").
#' @export
mc_grant <- function(conn, address, permissions) {
  mc_rpc(conn, "grant", list(address, permissions))
}

#' Grant permissions from a specific address
#' 
#' @param conn A multichain_conn object.
#' @param from_address Address from which the permissions are granted (must have admin rights).
#' @param to_address Target wallet address.
#' @param permissions Permission string (e.g., "connect,send,receive").
#' @param native_amount Optional native currency amount to send with the grant.
#' @param start_block Optional block from which the permission is valid.
#' @param end_block Optional block at which the permission expires.
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
#' @param conn A multichain_conn object.
#' @param to_address Target wallet address.
#' @param permissions Permission string.
#' @param data Data string or list (will be converted to hex).
#' @param native_amount Optional native currency amount.
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
#' @param conn A multichain_conn object.
#' @param from_address Address from which the permissions are granted.
#' @param to_address Target wallet address.
#' @param permissions Permission string.
#' @param data Data string or list (will be converted to hex).
#' @param native_amount Optional native currency amount.
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
#' @param conn A multichain_conn object.
#' @param permissions Permission type to filter (default "*").
#' @export
mc_list_permissions <- function(conn, permissions = "*") {
  res <- mc_rpc(conn, "listpermissions", list(permissions))
  rpc_res_to_df(res)
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

#' Revoke permissions from a specific address
#' 
#' @param conn A multichain_conn object.
#' @param from_address Address from which the revocation is issued.
#' @param to_address Target wallet address.
#' @param permissions Permission string to revoke.
#' @param native_amount Optional native currency amount.
#' @export
mc_revoke_from <- function(conn, from_address, to_address, permissions, native_amount = 0) {
  params <- list(from_address, to_address, permissions, native_amount)
  mc_rpc(conn, "revokefrom", params)
}

#' Verify if an address has a specific permission
#' 
#' @param conn A multichain_conn object.
#' @param address The address to check.
#' @param permission The permission name (e.g., "send", "admin").
#' @return Logical TRUE if the address has the permission, FALSE otherwise.
#' @export
mc_verify_permission <- function(conn, address, permission) {
  res <- mc_rpc(conn, "verifypermission", list(address, permission))
  return(as.logical(res))
}
