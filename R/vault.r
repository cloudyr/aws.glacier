#' @rdname vaults
#' @title Vaults
#' @description Create, describe, or delete vaults
#' @template vault
#' @template dots
#' @export
create_vault <- function(vault, ...) {
    if (nchar(vault) > 255) {
        stop("'vault' must be max 255 characters")
    }
    r <- glacierHTTP("PUT", paste0("/-/vaults/", vault), ...)
    return(r)
}

#' @rdname vaults
#' @export
delete_vault <- function(vault, ...) {
    r <- glacierHTTP("DELETE", paste0("/-/vaults/", vault), ...)
    return(r)
}

#' @rdname vaults
#' @export
describe_vault <- function(vault, ...) {
    r <- glacierHTTP("GET", paste0("/-/vaults/", vault), ...)
    return(r)
}

#' @title list_vaults
#' @description List vaults
#' @param n An integer specifying the number of vaults to return. The default (and maximum) is 1000.
#' @template marker
#' @template dots
#' @export
list_vaults <- function(n = 1000L, marker, ...) {
    query <- list()
    if (!n %in% 1:1000) {
        stop("'n' must be between 1 and 1000")
    }
    query$limit <- n
    if (!missing(marker)) {
        query$marker <- marker
    }
    r <- glacierHTTP("GET", "/-/vaults", ...)
    return(r)
}

#' @rdname notifications
#' @title Vault notifications
#' @description Get, set, and delete vault notifications
#' @template vault
#' @param events A character vector of events that will trigger notifications to the AWS SNS topic. Valid values are: \dQuote{ArchiveRetrievalCompleted} and \dQuote{InventoryRetrievalCompleted}.
#' @template topic
#' @template dots
#' @export
get_vault_notification <- function(vault, ...) {
    r <- glacierHTTP("GET", paste0("/-/vaults/", vault, "/notification-configuration"), ...)
    return(r)
}

#' @rdname notifications
#' @importFrom jsonlite toJSON
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

#' @rdname notifications
#' @export
delete_vault_notification <- function(vault, ...) {
    r <- glacierHTTP("DELETE", paste0("/-/vaults/", vault, "/notification-configuration"), ...)
    return(r)
}
