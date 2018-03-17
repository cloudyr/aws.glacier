# simple upload

#' @rdname archives
#' @title Archives
#' @description Upload or delete Archives
#' @template vault
#' @param contents This is the body to be uploaded, possibly the return value of \code{\link[httr]{upload_file}}.
#' @template archive 
#' @template dots
#' @export
upload_archive <- function(vault, contents, ...) {
    vault <- get_vault_name(vault)
    action <- paste0("/-/vaults/", vault, "/archives")
    r <- glacierHTTP("POST", action = action, body = contents, ...)
    return(r)
}

#' @rdname archives
#' @export
delete_archive <- function(vault, archive, ...) {
    vault <- get_vault_name(vault)
    action <- paste0("/-/vaults/", vault, "/archives/", archive)
    r <- glacierHTTP("DELETE", action = action, ...)
    return(r)
}


# multi-part upload
# should this be an R6 class instead?

initiate_upload <- function() {}

abort_upload <- function(vault, id, ...) {
    vault <- get_vault_name(vault)
    action <- paste0("/-/vaults/", vault, "/multipart-uploads/", id)
    r <- glacierHTTP("DELETE", action = action, ...)
    return(r)
}

complete_upload <- function(vault, id, ...) {
}

list_uploads <- function(vault, ...) {
    vault <- get_vault_name(vault)
    action <- paste0("/-/vaults/", vault, "/multipart-uploads/")
    r <- glacierHTTP("GET", action = action, ...)
    return(r)
}

upload_part <- function() {}

list_parts <- function(vault, id, ...) {
    vault <- get_vault_name(vault)
    action <- paste0("/-/vaults/", vault, "/multipart-uploads/", id)
    r <- glacierHTTP("GET", action = action, ...)
    return(r)
}
