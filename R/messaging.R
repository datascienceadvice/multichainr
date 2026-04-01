#' Sign a message with a private key
#'
#' Generates a base64‑encoded digital signature for a message using a private key.
#' The signature proves that the message was approved by the owner of the address
#' or the holder of the private key.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param address_or_key Character string. Either a wallet address (must belong
#'   to the node's wallet) or a private key in Wallet Import Format (WIF).
#' @param message Character string. The text message to sign.
#'
#' @return A character string containing the base64‑encoded signature.
#'
#' @examples
#' \dontrun{
#' # Sign using an address in the wallet
#' sig <- mc_sign_message(conn, "1A...", "Hello, MultiChain!")
#'
#' # Sign using a raw private key
#' sig <- mc_sign_message(conn, "L5...", "Important agreement")
#' }
#'
#' @seealso \code{\link{mc_verify_message}} to verify a signature,
#'   \code{\link{mc_get_new_address}} to generate a new address.
#'
#' @family cryptography
#' @export
mc_sign_message <- function(conn, address_or_key, message) {
  mc_rpc(conn, "signmessage", list(address_or_key, as.character(message)))
}

#' Verify a signed message
#'
#' Checks whether a message was signed by the owner of a given address.
#' The signature must have been created by \code{\link{mc_sign_message}}.
#'
#' @param conn A connection object created by \code{\link{mc_connect}}.
#' @param address Character string. The address that allegedly signed the message.
#' @param signature Character string. The base64‑encoded signature (as returned
#'   by \code{\link{mc_sign_message}}).
#' @param message Character string. The original text message.
#'
#' @return A logical value: \code{TRUE} if the signature is valid,
#'   \code{FALSE} otherwise.
#'
#' @examples
#' \dontrun{
#' # Sign a message
#' sig <- mc_sign_message(conn, "1A...", "Hello")
#' # Verify it
#' valid <- mc_verify_message(conn, "1A...", sig, "Hello")
#' print(valid)  # should be TRUE
#' }
#'
#' @seealso \code{\link{mc_sign_message}} to create a signature.
#'
#' @family cryptography
#' @export
mc_verify_message <- function(conn, address, signature, message) {
  res <- mc_rpc(conn, "verifymessage", list(address, signature, as.character(message)))
  as.logical(res)
}
