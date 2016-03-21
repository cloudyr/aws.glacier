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
#' @param n
#' @template dots
#' @export
list_vaults <- function(n, ...) {
    r <- glacierHTTP("GET", "/-/vaults", ...)
    return(r)
}

#' @rdname notifications
#' @title Vault notifications
#' @description Get, set, and delete vault notifications
#' @template vault
#' @param events
#' @param sns_topic A character string specifying an SNS topic name or ARN.
#' @template dots
#' @export
get_vault_notification <- function(vault, ...) {
    r <- glacierHTTP("GET", paste0("/-/vaults/", vault, "/notification-configuration"), ...)
    return(r)
}

#' @rdname notifications
#' @importFrom jsonlite toJSON
#' @export
set_vault_notification <- function(vault, events, sns_topic, ...) {
    b <- list()
    vevents <- c("ArchiveRetrievalCompleted", "InventoryRetrievalCompleted")
    if (any(!events %in% vevents)) {
        stop("'events' must be in: ", paste0(vevents, collapse = ", "))
    }
    b$Events <- events
    b$SNSTopic <- sns_topic
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
