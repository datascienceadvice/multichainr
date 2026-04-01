#' Create a stream filter
#' @param conn Connection object.
#' @param name Filter name.
#' @param options List of options, e.g., list(libraries = list("lib1")).
#' @param js_code JavaScript code for the filter.
#' @export
mc_create_stream_filter <- function(conn, name, options, js_code) {
  mc_rpc(conn, "create", list("streamfilter", name, options, js_code))
}

#' Create a transaction filter
#' @param conn Connection object.
#' @param name Filter name.
#' @param options List of options, e.g., list(for = "asset1", libraries = list("lib1")).
#' @param js_code JavaScript code for the filter.
#' @export
mc_create_tx_filter <- function(conn, name, options, js_code) {
  mc_rpc(conn, "create", list("txfilter", name, options, js_code))
}

#' Create a blockchain upgrade
#' @param conn Connection object.
#' @param name Upgrade name.
#' @param params List of parameters to upgrade (e.g., list("target-block-time" = 20)).
#' @export
mc_create_upgrade <- function(conn, name, params) {
  mc_rpc(conn, "create", list("upgrade", name, FALSE, params))
}

#' Approve or disapprove an upgrade or filter
#' @param conn Connection object.
#' @param from_address Admin address.
#' @param entity Name or txid of the upgrade/filter.
#' @param approve Boolean (for global) or list(for = "stream", approve = TRUE) for stream filters.
#' @export
mc_approve_from <- function(conn, from_address, entity, approve) {
  mc_rpc(conn, "approvefrom", list(from_address, entity, approve))
}

#' Get JavaScript code of a filter
#' @param conn Connection object.
#' @param filter Filter.
#' @export
mc_get_filter_code <- function(conn, filter) {
  mc_rpc(conn, "getfiltercode", list(filter))
}

#' List stream filters
#' @param conn Connection object.
#' @param filters Filters.
#' @export
mc_list_stream_filters <- function(conn, filters = "*", verbose = FALSE) {
  res <- mc_rpc(conn, "liststreamfilters", list(filters, verbose))
  rpc_res_to_df(res)
}

#' List transaction filters
#' @export
mc_list_tx_filters <- function(conn, filters = "*", verbose = FALSE) {
  res <- mc_rpc(conn, "listtxfilters", list(filters, verbose))
  rpc_res_to_df(res)
}

#' List upgrades
#' @export
mc_list_upgrades <- function(conn, upgrades = "*") {
  res <- mc_rpc(conn, "listupgrades", list(upgrades))
  rpc_res_to_df(res)
}

#' Test a transaction filter before creation
#' @param options Filter options.
#' @param js_code JavaScript code.
#' @param tx Hex or txid of a transaction to test against.
#' @export
mc_test_tx_filter <- function(conn, options, js_code, tx = NULL) {
  params <- list(options, js_code)
  if (!is.null(tx)) params <- c(params, list(tx))
  mc_rpc(conn, "testtxfilter", params)
}

#' Run an existing transaction filter against a transaction
#' @export
mc_run_tx_filter <- function(conn, filter, tx) {
  mc_rpc(conn, "runtxfilter", list(filter, tx))
}

#' Test a stream filter before creation
#' @param vout Optional output index.
#' @export
mc_test_stream_filter <- function(conn, options, js_code, tx = NULL, vout = NULL) {
  params <- list(options, js_code)
  if (!is.null(tx)) {
    params <- c(params, list(tx))
    if (!is.null(vout)) params <- c(params, list(as.integer(vout)))
  }
  mc_rpc(conn, "teststreamfilter", params)
}

#' Run an existing stream filter against an item
#' @export
mc_run_stream_filter <- function(conn, filter, tx, vout = NULL) {
  params <- list(filter, tx)
  if (!is.null(vout)) params <- c(params, list(as.integer(vout)))
  mc_rpc(conn, "runstreamfilter", params)
}

# --- Библиотеки (Libraries) ---

#' Create a new library
#' @param updatemode One of "none", "instant", or "approve".
#' @export
mc_create_library <- function(conn, name, updatemode = c("none", "instant", "approve"), js_code) {
  updatemode <- match.arg(updatemode)
  mc_rpc(conn, "create", list("library", name, list(updatemode = updatemode), js_code))
}

#' Add an update to an existing library
#' @export
mc_add_library_update <- function(conn, library, updatename, js_code) {
  mc_rpc(conn, "addlibraryupdate", list(library, updatename, js_code))
}

#' Add an update to a library from a specific address
#' @export
mc_add_library_update_from <- function(conn, from_address, library, updatename, js_code) {
  mc_rpc(conn, "addlibraryupdatefrom", list(from_address, library, updatename, js_code))
}

#' Get JavaScript code for a library
#' @param updatename Optional. If omitted, returns active code. Use "" for initial code.
#' @export
mc_get_library_code <- function(conn, library, updatename = NULL) {
  params <- list(library)
  if (!is.null(updatename)) params <- c(params, list(updatename))
  mc_rpc(conn, "getlibrarycode", params)
}

#' List libraries on the blockchain
#' @export
mc_list_libraries <- function(conn, libraries = "*", verbose = FALSE) {
  res <- mc_rpc(conn, "listlibraries", list(libraries, verbose))
  rpc_res_to_df(res)
}

#' Manage testing of libraries and updates locally
#' @export
mc_test_library <- function(conn, library = NULL, updatename = NULL, js_code = NULL) {
  params <- list()
  if (!is.null(library)) {
    params <- list(library)
    if (!is.null(updatename)) params <- c(params, list(updatename))
    if (!is.null(js_code)) params <- c(params, list(js_code))
  }
  mc_rpc(conn, "testlibrary", params)
}

# --- Переменные (Variables) ---

#' Create a new variable
#' @param open If TRUE, anyone with create permissions can edit.
#' @param value Initial JSON value.
#' @export
mc_create_variable <- function(conn, name, open = TRUE, value = NULL) {
  mc_rpc(conn, "create", list("variable", name, open, value))
}

#' Create a variable from a specific address
#' @export
mc_create_variable_from <- function(conn, from_address, name, open = TRUE, value = NULL) {
  mc_rpc(conn, "createfrom", list(from_address, "variable", name, open, value))
}

#' Set the value of a variable
#' @param value Any valid JSON structure (list, string, number).
#' @export
mc_set_variable_value <- function(conn, variable, value = NULL) {
  mc_rpc(conn, "setvariablevalue", list(variable, value))
}

#' Set variable value from specific address
#' @export
mc_set_variable_value_from <- function(conn, from_address, variable, value = NULL) {
  mc_rpc(conn, "setvariablevaluefrom", list(from_address, variable, value))
}

#' Retrieve the latest value of a variable
#' @export
mc_get_variable_value <- function(conn, variable) {
  mc_rpc(conn, "getvariablevalue", list(variable))
}

#' Get information about a variable
#' @export
mc_get_variable_info <- function(conn, variable, verbose = FALSE) {
  mc_rpc(conn, "getvariableinfo", list(variable, verbose))
}

#' List historical values of a variable
#' @export
mc_get_variable_history <- function(conn, variable, verbose = FALSE, count = 10, start = NULL) {
  actual_start <- if (is.null(start)) -as.integer(count) else as.integer(start)
  res <- mc_rpc(conn, "getvariablehistory", list(variable, verbose, as.integer(count), actual_start))
  rpc_res_to_df(res)
}

#' List variables created on the blockchain
#' @export
mc_list_variables <- function(conn, variables = "*", verbose = FALSE, count = NULL, start = NULL) {
  params <- list(variables, verbose)
  if (!is.null(count)) params <- c(params, list(as.integer(count)))
  if (!is.null(start)) {
    if (is.null(count)) params <- c(params, list(2147483647))
    params <- c(params, list(as.integer(start)))
  }
  res <- mc_rpc(conn, "listvariables", params)
  rpc_res_to_df(res)
}