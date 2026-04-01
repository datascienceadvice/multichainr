#' Sign a message with a private key
#' 
#' Returns a base64-encoded digital signature which proves that the message 
#' was approved by the owner of the address or private key.
#' 
#' @param conn Connection object.
#' @param address_or_key A wallet address (must be in this wallet) or a private key (WIF).
#' @param message The text message to sign.
#' @return A string containing the base64-encoded signature.
#' @export
mc_sign_message <- function(conn, address_or_key, message) {
  mc_rpc(conn, "signmessage", list(address_or_key, as.character(message)))
}

#' Verify a signed message
#' 
#' Checks if a message was signed by the owner of a specific address.
#' 
#' @param conn Connection object.
#' @param address The address to verify against.
#' @param signature The base64-encoded signature (from mc_sign_message).
#' @param message The original text message.
#' @return Logical TRUE if the signature is valid, FALSE otherwise.
#' @export
mc_verify_message <- function(conn, address, signature, message) {
  res <- mc_rpc(conn, "verifymessage", list(address, signature, as.character(message)))
  as.logical(res)
}