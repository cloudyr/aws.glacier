#' @rdname retrieval_policies
#' @title Data Retrieval policies
#' @description Get and set data retrieval policies
#' @param strategy A character string specifying the type of data retrieval policy to set. Must be one of: \dQuote{BytesPerHour}, \dQuote{FreeTier}, or \dQuote{None}.
#' @param bytes If \code{strategy = "BytesPerHour"}, an integer specifying the maximum number of bytes that can be retrieved per hour (between 1 and \eqn{2^63-1}).
#' @template dots
#' @return A list.
#' @export
get_retrieval_policy <- function(...) {
    r <- glacierHTTP("GET", "/-/policies/data-retrieval", ...)
    return(r)
}

#' @rdname retrieval_policies
#' @importFrom jsonlite toJSON
#' @export
set_retrieval_policy <- function(strategy, bytes, ...) {
    # parse body to JSON
    b <- list()
    b$Policy <- list()
    b$Policy$Rules <- list()
    vstrat <- c("BytesPerHour","FreeTier","None")
    if (!strategy %in% vstrat) {
        stop("'vstrat' must be in: ", paste0(vstrat, collapse = ", "))
    }
    b$Policy$Rules <- c("Strategy" = strategy)
    if (strategy == "BytesPerHour") {
        bytes <- as.integer(bytes)
        if (bytes < 1 | bytes > (2^63-1)) {
            stop("'bytes' must be an integer between 1 and 2^63-1")
        }
        b$Policy$Rules <- c(b$Policy$Rules$Strategy, "BytesPerHour" = bytes)
    }
    b <- toJSON(b)
    r <- glacierHTTP("PUT", "/-/policies/data-retrieval", body = b, ...)
    return(r)
}
