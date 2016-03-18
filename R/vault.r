#' @export
create_vault <- function(vault, ...) {
    if (nchar(vault) > 255) {
        stop("'vault' must be max 255 characters")
    }
    r <- glacierHTTP("PUT", paste0("/-/vaults/", vault), ...)
    return(r)
}

#' @export
delete_vault <- function(vault, ...) {
    r <- glacierHTTP("DELETE", paste0("/-/vaults/", vault), ...)
    return(r)
}

#' @export
describe_vault <- function(vault, ...) {
    r <- glacierHTTP("GET", paste0("/-/vaults/", vault), ...)
    return(r)
}

#' @export
list_vaults <- function(n, ...) {
    r <- glacierHTTP("GET", "/-/vaults", ...)
    return(r)
}

#' @export
get_vault_notification <- function(vault, ...) {
    r <- glacierHTTP("GET", paste0("/-/vaults/", vault, "/notification-configuration"), ...)
    return(r)
}

#' @export
set_vault_notification <- function(vault, events, topic, ...) {
    b <- list()
    vevents <- c("ArchiveRetrievalCompleted", "InventoryRetrievalCompleted")
    if (any(!events %in% vevents)) {
        stop("'events' must be in: ", paste0(vevents, collapse = ", "))
    }
    b$Events <- events
    b$SNSTopic <- topic
    b <- toJSON(b)
    r <- glacierHTTP("PUT", paste0("/-/vaults/", vault, "/notification-configuration"), 
                     body = b, ...)
    return(r)
}

#' @export
delete_vault_notification <- function(vault, ...) {
    r <- glacierHTTP("DELETE", paste0("/-/vaults/", vault, "/notification-configuration"), ...)
    return(r)
}
