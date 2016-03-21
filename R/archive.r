# simple upload

#' @rdname archives
#' @title Archives
#' @description Upload or delete Archives
#' @template vault
#' @param contents 
#' @param archive
#' @template dots
#' @export
upload_archive <- function(vault, contents, ...) {
    action <- paste0("/-/vaults/", vault, "/archives")
    r <- glacierHTTP("POST", action = action, body = contents, ...)
    return(r)
}

#' @rdname archives
#' @export
delete_archive <- function(vault, archive, ...) {
    action <- paste0("/-/vaults/", vault, "/archives/", archive)
    r <- glacierHTTP("DELETE", action = action, ...)
    return(r)
}


# multi-part upload
# should this be an R6 class instead?

initiate_upload <- function() {}

abort_upload <- function(vault, id, ...) {
    action <- paste0("/-/vaults/", vault, "/multipart-uploads/", id)
    r <- glacierHTTP("DELETE", action = action, ...)
    return(r)
}

complete_upload <- function(vault, id, ...) {
}

list_uploads <- function(vault, ...) {
    action <- paste0("/-/vaults/", vault, "/multipart-uploads/")
    r <- glacierHTTP("GET", action = action, ...)
    return(r)
}

upload_part <- function() {}

list_parts <- function(vault, id, ...) {
    action <- paste0("/-/vaults/", vault, "/multipart-uploads/", id)
    r <- glacierHTTP("GET", action = action, ...)
    return(r)
}
