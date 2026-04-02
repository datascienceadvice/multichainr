prep_filter_options <- function(options) {
  if (is.list(options) && !is.null(options$libraries)) {
    # Если передана одна строка, превращаем ее в список, 
    # чтобы jsonlite сохранил ее как JSON array []
    if (is.character(options$libraries) && length(options$libraries) == 1) {
      options$libraries <- as.list(options$libraries)
    }
  }
  return(options)
}

#' Create a stream filter
#'
#' Creates a new stream filter on the blockchain. Stream filters are JavaScript
#' programs that can be attached to streams to validate or transform items.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param name Character string. Name of the filter (must be unique).
#' @param options List of filter options. Typically includes \code{libraries}
#'   (list of library names) that the filter depends on. Example:
#'   \code{list(libraries = list("mylib"))}.
#' @param js_code Character string. The JavaScript code for the filter.
#'
#' @return A list containing the result of the RPC call (usually transaction ID).
#'
#' @examples
#' \dontrun{
#' js_code <- "function filter(stream, item) { return true; }"
#' mc_create_stream_filter(conn, "myfilter", list(libraries = list()), js_code)
#' }
#'
#' @seealso \code{\link{mc_create_tx_filter}}, \code{\link{mc_list_stream_filters}}
#'
#' @family filters
#' @export
mc_create_stream_filter <- function(conn, name, options, js_code) {
  options <- prep_filter_options(options)
  mc_rpc(conn, "create", list("streamfilter", name, options, js_code))
}

#' Create a transaction filter
#'
#' Creates a new transaction filter on the blockchain. Transaction filters are
#' JavaScript programs that validate or transform transactions.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param name Character string. Name of the filter (must be unique).
#' @param options List of filter options. Usually includes \code{for} (target
#'   asset or stream) and \code{libraries} (list of library names). Example:
#'   \code{list(for = "asset1", libraries = list("lib1"))}.
#' @param js_code Character string. The JavaScript code for the filter.
#'
#' @return A list containing the result of the RPC call (usually transaction ID).
#'
#' @examples
#' \dontrun{
#' js_code <- "function filter(tx) { return tx.vin.length > 0; }"
#' mc_create_tx_filter(conn, "myfilter", list(for = "asset1"), js_code)
#' }
#'
#' @seealso \code{\link{mc_create_stream_filter}}, \code{\link{mc_list_tx_filters}}
#'
#' @family filters
#' @export
mc_create_tx_filter <- function(conn, name, options, js_code) {
  options <- prep_filter_options(options)
  mc_rpc(conn, "create", list("txfilter", name, options, js_code))
}

#' Create a blockchain upgrade
#'
#' Creates a new upgrade proposal to change blockchain parameters (e.g., target
#' block time, maximum block size). Upgrades require admin approval.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param name Character string. Name of the upgrade (must be unique).
#' @param params List of parameters to upgrade, e.g.,
#'   \code{list("target-block-time" = 20, "max-block-size" = 10000000)}.
#'
#' @return A list containing the result of the RPC call (usually transaction ID).
#'
#' @examples
#' \dontrun{
#' mc_create_upgrade(conn, "speedup", list("target-block-time" = 20))
#' }
#'
#' @seealso \code{\link{mc_approve_from}} to approve an upgrade.
#'
#' @family filters
#' @export
mc_create_upgrade <- function(conn, name, params) {
  mc_rpc(conn, "create", list("upgrade", name, FALSE, params))
}

#' Approve or disapprove an upgrade or filter
#'
#' Sends an approval or disapproval transaction from a specific address
#' (must have admin permissions). This is used to vote on upgrades or
#' to approve/disapprove filters.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. Admin address that issues the approval.
#' @param entity Character string. Name or transaction ID of the upgrade/filter.
#' @param approve Either a logical (for global upgrades/filters) or a list
#'   of the form \code{list(for = "stream", approve = TRUE)} for stream‑specific
#'   filter approval.
#'
#' @return A list containing the result of the RPC call (usually transaction ID).
#'
#' @examples
#' \dontrun{
#' # Approve a global upgrade
#' mc_approve_from(conn, "admin_address", "speedup", approve = TRUE)
#'
#' # Approve a stream filter for a specific stream
#' mc_approve_from(conn, "admin_address", "myfilter",
#'                 approve = list(for = "mystream", approve = TRUE))
#' }
#'
#' @seealso \code{\link{mc_create_upgrade}}, \code{\link{mc_create_stream_filter}}
#'
#' @family filters
#' @export
mc_approve_from <- function(conn, from_address, entity, approve) {
  mc_rpc(conn, "approvefrom", list(from_address, entity, approve))
}

#' Get JavaScript code of a filter
#'
#' Retrieves the JavaScript code (and associated metadata) of an existing
#' stream or transaction filter.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param filter Character string. Filter name or transaction ID.
#'
#' @return A list containing the filter's code and details.
#'
#' @examples
#' \dontrun{
#' code_info <- mc_get_filter_code(conn, "myfilter")
#' print(code_info$code)
#' }
#'
#' @seealso \code{\link{mc_get_library_code}} for libraries.
#'
#' @family filters
#' @export
mc_get_filter_code <- function(conn, filter) {
  mc_rpc(conn, "getfiltercode", list(filter))
}

#' List stream filters
#'
#' Returns a list of stream filters on the blockchain, with optional filtering
#' and verbosity.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param filters Character vector of filter names/IDs, or \code{"*"} (default)
#'   for all filters.
#' @param verbose Logical. If \code{TRUE}, returns detailed information.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with filter information.
#'
#' @examples
#' \dontrun{
#' all_filters <- mc_list_stream_filters(conn)
#' }
#'
#' @seealso \code{\link{mc_create_stream_filter}}, \code{\link{mc_list_tx_filters}}
#'
#' @family filters
#' @export
mc_list_stream_filters <- function(conn, filters = "*", verbose = FALSE) {
  res <- mc_rpc(conn, "liststreamfilters", list(filters, verbose))
  rpc_res_to_df(res)
}

#' List transaction filters
#'
#' Returns a list of transaction filters on the blockchain.
#'
#' @inheritParams mc_list_stream_filters
#'
#' @return A data frame (via \code{rpc_res_to_df}) with filter information.
#'
#' @examples
#' \dontrun{
#' tx_filters <- mc_list_tx_filters(conn)
#' }
#'
#' @family filters
#' @export
mc_list_tx_filters <- function(conn, filters = "*", verbose = FALSE) {
  res <- mc_rpc(conn, "listtxfilters", list(filters, verbose))
  rpc_res_to_df(res)
}

#' List upgrades
#'
#' Returns a list of upgrade proposals on the blockchain.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param upgrades Character vector of upgrade names/IDs, or \code{"*"} (default)
#'   for all upgrades.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with upgrade information.
#'
#' @examples
#' \dontrun{
#' upgrades <- mc_list_upgrades(conn)
#' }
#'
#' @family filters
#' @export
mc_list_upgrades <- function(conn, upgrades = "*") {
  res <- mc_rpc(conn, "listupgrades", list(upgrades))
  rpc_res_to_df(res)
}

#' Test a transaction filter before creation
#'
#' Tests a transaction filter's JavaScript code against a given transaction
#' without permanently creating the filter.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param options List of filter options (similar to \code{mc_create_tx_filter}).
#' @param js_code Character string. JavaScript code to test.
#' @param tx Optional character string. Hex representation or transaction ID
#'   of a transaction to test against. If omitted, a generic test is performed.
#'
#' @return The result of the filter evaluation (typically a boolean or
#'   transformed transaction).
#'
#' @examples
#' \dontrun{
#' result <- mc_test_tx_filter(conn, list(for = "asset1"),
#'                             "function filter(tx) { return true; }",
#'                             tx = "txid...")
#' }
#'
#' @seealso \code{\link{mc_create_tx_filter}}, \code{\link{mc_run_tx_filter}}
#'
#' @family filters
#' @export
mc_test_tx_filter <- function(conn, options, js_code, tx = NULL) {
  options <- prep_filter_options(options)
  params <- list(options, js_code)
  if (!is.null(tx)) params <- c(params, list(tx))
  mc_rpc(conn, "testtxfilter", params)
}

#' Run an existing transaction filter against a transaction
#'
#' Executes an existing transaction filter on a given transaction, without
#' performing a blockchain operation.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param filter Character string. Filter name or transaction ID.
#' @param tx Character string. Transaction ID or hex representation.
#'
#' @return The output of the filter (e.g., boolean, transformed transaction).
#'
#' @examples
#' \dontrun{
#' result <- mc_run_tx_filter(conn, "myfilter", "txid...")
#' }
#'
#' @seealso \code{\link{mc_test_tx_filter}}, \code{\link{mc_create_tx_filter}}
#'
#' @family filters
#' @export
mc_run_tx_filter <- function(conn, filter, tx) {
  mc_rpc(conn, "runtxfilter", list(filter, tx))
}

#' Test a stream filter before creation
#'
#' Tests a stream filter's JavaScript code against a specific stream item
#' (transaction and optional vout) without permanently creating the filter.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param options List of filter options (similar to \code{mc_create_stream_filter}).
#' @param js_code Character string. JavaScript code to test.
#' @param tx Optional character string. Transaction ID or hex representation
#'   of the stream item's transaction.
#' @param vout Optional integer. Output index if the stream item is in a
#'   transaction output.
#'
#' @return The result of the filter evaluation.
#'
#' @examples
#' \dontrun{
#' result <- mc_test_stream_filter(conn, list(libraries = list()),
#'                                 "function filter(stream, item) { return true; }",
#'                                 tx = "txid...", vout = 0)
#' }
#'
#' @seealso \code{\link{mc_create_stream_filter}}, \code{\link{mc_run_stream_filter}}
#'
#' @family filters
#' @export
mc_test_stream_filter <- function(conn, options, js_code, tx = NULL, vout = NULL) {
  options <- prep_filter_options(options)
  params <- list(options, js_code)
  if (!is.null(tx)) {
    params <- c(params, list(tx))
    if (!is.null(vout)) params <- c(params, list(as.integer(vout)))
  }
  mc_rpc(conn, "teststreamfilter", params)
}

#' Run an existing stream filter against an item
#'
#' Executes an existing stream filter on a specific stream item (transaction
#' and optional vout).
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param filter Character string. Filter name or transaction ID.
#' @param tx Character string. Transaction ID or hex representation.
#' @param vout Optional integer. Output index if the stream item is in a
#'   transaction output.
#'
#' @return The output of the filter.
#'
#' @examples
#' \dontrun{
#' result <- mc_run_stream_filter(conn, "myfilter", "txid...", vout = 0)
#' }
#'
#' @seealso \code{\link{mc_test_stream_filter}}, \code{\link{mc_create_stream_filter}}
#'
#' @family filters
#' @export
mc_run_stream_filter <- function(conn, filter, tx, vout = NULL) {
  params <- list(filter, tx)
  if (!is.null(vout)) params <- c(params, list(as.integer(vout)))
  mc_rpc(conn, "runstreamfilter", params)
}

# --- Библиотеки (Libraries) ---

#' Create a new library
#'
#' Creates a JavaScript library on the blockchain. Libraries contain reusable
#' code that can be imported by filters.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param name Character string. Library name (must be unique).
#' @param updatemode Character string. How library updates are handled:
#'   \code{"none"} – no updates allowed; \code{"instant"} – updates take effect
#'   immediately; \code{"approve"} – updates require admin approval.
#' @param js_code Character string. The JavaScript code for the library.
#'
#' @return A list containing the result of the RPC call (usually transaction ID).
#'
#' @examples
#' \dontrun{
#' js_code <- "function add(a, b) { return a + b; }"
#' mc_create_library(conn, "math", updatemode = "none", js_code)
#' }
#'
#' @seealso \code{\link{mc_add_library_update}}, \code{\link{mc_list_libraries}}
#'
#' @family libraries
#' @export
mc_create_library <- function(conn, name, updatemode = c("none", "instant", "approve"), js_code) {
  updatemode <- match.arg(updatemode)
  mc_rpc(conn, "create", list("library", name, list(updatemode = updatemode), js_code))
}

#' Add an update to an existing library
#'
#' Adds a new version (update) to an existing library. The update mechanism
#' depends on the library's \code{updatemode}.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param library Character string. Library name or transaction ID.
#' @param updatename Character string. Name of this update (must be unique).
#' @param js_code Character string. The new JavaScript code (or patch).
#'
#' @return A list containing the result of the RPC call (usually transaction ID).
#'
#' @examples
#' \dontrun{
#' mc_add_library_update(conn, "math", "v2", "function add(a, b) { return a + b + 1; }")
#' }
#'
#' @seealso \code{\link{mc_create_library}}, \code{\link{mc_add_library_update_from}}
#'
#' @family libraries
#' @export
mc_add_library_update <- function(conn, library, updatename, js_code) {
  mc_rpc(conn, "addlibraryupdate", list(library, updatename, js_code))
}

#' Add an update to a library from a specific address
#'
#' Similar to \code{\link{mc_add_library_update}}, but allows specifying the
#' sending address.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. Address that pays for and issues the update.
#' @param library Character string. Library name or transaction ID.
#' @param updatename Character string. Name of this update.
#' @param js_code Character string. The new JavaScript code (or patch).
#'
#' @return A list containing the result of the RPC call (usually transaction ID).
#'
#' @examples
#' \dontrun{
#' mc_add_library_update_from(conn, "1A...", "math", "v2", "function add(a, b) { return a + b; }")
#' }
#'
#' @family libraries
#' @export
mc_add_library_update_from <- function(conn, from_address, library, updatename, js_code) {
  mc_rpc(conn, "addlibraryupdatefrom", list(from_address, library, updatename, js_code))
}

#' Get JavaScript code for a library
#'
#' Retrieves the JavaScript code of a library. By default, returns the active
#' code. If \code{updatename} is provided, returns the code of that specific
#' update (or the initial code if \code{updatename = ""}).
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param library Character string. Library name or transaction ID.
#' @param updatename Optional character string. Update name. If omitted,
#'   returns the active code. Use \code{""} to retrieve the initial code.
#'
#' @return A list containing the library code and metadata.
#'
#' @examples
#' \dontrun{
#' active_code <- mc_get_library_code(conn, "math")
#' initial_code <- mc_get_library_code(conn, "math", updatename = "")
#' }
#'
#' @seealso \code{\link{mc_get_filter_code}} for filters.
#'
#' @family libraries
#' @export
mc_get_library_code <- function(conn, library, updatename = NULL) {
  params <- list(library)
  if (!is.null(updatename)) params <- c(params, list(updatename))
  mc_rpc(conn, "getlibrarycode", params)
}

#' List libraries on the blockchain
#'
#' Returns a list of libraries with optional filtering and verbosity.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param libraries Character vector of library names/IDs, or \code{"*"} (default)
#'   for all libraries.
#' @param verbose Logical. If \code{TRUE}, returns detailed information.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with library information.
#'
#' @examples
#' \dontrun{
#' libs <- mc_list_libraries(conn)
#' }
#'
#' @family libraries
#' @export
mc_list_libraries <- function(conn, libraries = "*", verbose = FALSE) {
  res <- mc_rpc(conn, "listlibraries", list(libraries, verbose))
  rpc_res_to_df(res)
}

#' Manage testing of libraries and updates locally
#'
#' Tests a library's code or a specific update without permanently creating it.
#' The behaviour depends on the arguments:
#' \itemize{
#'   \item If only \code{js_code} is provided, tests that code as a new library.
#'   \item If \code{library} and optionally \code{updatename} are given, tests
#'         the existing library's code (or a specific update).
#' }
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param library Optional character string. Library name or transaction ID.
#' @param updatename Optional character string. Update name (if testing an update).
#' @param js_code Optional character string. JavaScript code (if testing a new library).
#'
#' @return The result of the test (e.g., compiled code, validation output).
#'
#' @examples
#' \dontrun{
#' # Test a new library
#' mc_test_library(conn, js_code = "function add(a, b) { return a + b; }")
#'
#' # Test an existing library's active code
#' mc_test_library(conn, library = "math")
#' }
#'
#' @seealso \code{\link{mc_create_library}}, \code{\link{mc_add_library_update}}
#'
#' @family libraries
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
#'
#' Creates a global variable on the blockchain. Variables are key‑value stores
#' that can be read by filters and transactions.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param name Character string. Variable name (must be unique).
#' @param open Logical. If \code{TRUE}, anyone with \code{create} permissions
#'   can edit the variable. If \code{FALSE}, only admins can edit.
#' @param value Optional. Initial JSON value (list, number, string, etc.).
#'
#' @return A list containing the result of the RPC call (usually transaction ID).
#'
#' @examples
#' \dontrun{
#' mc_create_variable(conn, "myvar", open = TRUE, value = list(key = "value"))
#' }
#'
#' @seealso \code{\link{mc_set_variable_value}}, \code{\link{mc_get_variable_value}}
#'
#' @family variables
#' @export
mc_create_variable <- function(conn, name, open = TRUE, value = NULL) {
  mc_rpc(conn, "create", list("variable", name, open, value))
}

#' Create a variable from a specific address
#'
#' Creates a global variable, specifying the address that issues the transaction.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param from_address Character string. Address that pays for and creates the variable.
#' @param name Character string. Variable name.
#' @param open Logical. If \code{TRUE}, anyone with \code{create} permissions
#'   can edit the variable.
#' @param value Optional. Initial JSON value.
#'
#' @return A list containing the result of the RPC call (usually transaction ID).
#'
#' @examples
#' \dontrun{
#' mc_create_variable_from(conn, "1A...", "myvar", open = TRUE, value = 42)
#' }
#'
#' @family variables
#' @export
mc_create_variable_from <- function(conn, from_address, name, open = TRUE, value = NULL) {
  mc_rpc(conn, "createfrom", list(from_address, "variable", name, open, value))
}

#' Set the value of a variable
#'
#' Updates the value of an existing variable. The new value can be any JSON‑compatible
#' object (list, number, string, etc.).
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param variable Character string. Variable name or transaction ID.
#' @param value Optional. New value (any JSON structure). If \code{NULL}, the
#'   variable is deleted.
#'
#' @return A list containing the result of the RPC call (usually transaction ID).
#'
#' @examples
#' \dontrun{
#' mc_set_variable_value(conn, "myvar", value = list(updated = TRUE))
#' }
#'
#' @seealso \code{\link{mc_create_variable}}, \code{\link{mc_get_variable_value}}
#'
#' @family variables
#' @export
mc_set_variable_value <- function(conn, variable, value = NULL) {
  mc_rpc(conn, "setvariablevalue", list(variable, value))
}

#' Set variable value from specific address
#'
#' Updates a variable's value, specifying the address that pays for the transaction.
#'
#' @inheritParams mc_set_variable_value
#' @param from_address Character string. Address that pays for the update.
#'
#' @return A list containing the result of the RPC call (usually transaction ID).
#'
#' @examples
#' \dontrun{
#' mc_set_variable_value_from(conn, "1A...", "myvar", value = 100)
#' }
#'
#' @family variables
#' @export
mc_set_variable_value_from <- function(conn, from_address, variable, value = NULL) {
  mc_rpc(conn, "setvariablevaluefrom", list(from_address, variable, value))
}

#' Retrieve the latest value of a variable
#'
#' Returns the current value of a variable.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param variable Character string. Variable name or transaction ID.
#'
#' @return The current value (any JSON type).
#'
#' @examples
#' \dontrun{
#' val <- mc_get_variable_value(conn, "myvar")
#' }
#'
#' @seealso \code{\link{mc_get_variable_info}}, \code{\link{mc_get_variable_history}}
#'
#' @family variables
#' @export
mc_get_variable_value <- function(conn, variable) {
  mc_rpc(conn, "getvariablevalue", list(variable))
}

#' Get information about a variable
#'
#' Returns metadata about a variable, such as creator, open status, creation time.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param variable Character string. Variable name or transaction ID.
#' @param verbose Logical. If \code{TRUE}, includes additional details.
#'
#' @return A list with variable information.
#'
#' @examples
#' \dontrun{
#' info <- mc_get_variable_info(conn, "myvar")
#' print(info$open)
#' }
#'
#' @family variables
#' @export
mc_get_variable_info <- function(conn, variable, verbose = FALSE) {
  mc_rpc(conn, "getvariableinfo", list(variable, verbose))
}

#' List historical values of a variable
#'
#' Retrieves the update history of a variable, showing previous values
#' and their transaction IDs.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param variable Character string. Variable name or transaction ID.
#' @param verbose Logical. If \code{TRUE}, returns detailed entries.
#' @param count Integer. Number of historical entries to return (default 10).
#' @param start Optional integer. Offset (positive for forward, negative for backward).
#'   If omitted, the most recent entries are returned.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with history entries.
#'
#' @examples
#' \dontrun{
#' history <- mc_get_variable_history(conn, "myvar", count = 5)
#' }
#'
#' @family variables
#' @export
mc_get_variable_history <- function(conn, variable, verbose = FALSE, count = 10, start = NULL) {
  actual_start <- if (is.null(start)) -as.integer(count) else as.integer(start)
  res <- mc_rpc(conn, "getvariablehistory", list(variable, verbose, as.integer(count), actual_start))
  rpc_res_to_df(res)
}

#' List variables created on the blockchain
#'
#' Returns a list of variables, with optional filtering, verbosity, and pagination.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param variables Character vector of variable names/IDs, or \code{"*"} (default)
#'   for all variables.
#' @param verbose Logical. If \code{TRUE}, returns detailed information.
#' @param count Optional integer. Maximum number of variables to return.
#' @param start Optional integer. Offset for pagination.
#'
#' @return A data frame (via \code{rpc_res_to_df}) with variable information.
#'
#' @examples
#' \dontrun{
#' all_vars <- mc_list_variables(conn)
#' first_10 <- mc_list_variables(conn, count = 10)
#' }
#'
#' @family variables
#' @export
mc_list_variables <- function(conn, variables = "*", verbose = FALSE, count = NULL, start = NULL) {
  params <- list(variables, verbose)
  if (!is.null(count)) params <- c(params, list(as.integer(count)))
  if (!is.null(start)) {
    if (is.null(count)) params <- c(params, list(2147483647)) # MAX_INT
    params <- c(params, list(as.integer(start)))
  }
  res <- mc_rpc(conn, "listvariables", params)
  rpc_res_to_df(res)
}
