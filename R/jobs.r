#' @title Initiate job
#' @description Initiate a Glacier job
#' @template dots
#' @export
initiate_job <- 
function(description,
         type,
         archive, 
         format,
         limit,
         marker,
         bytes, # two-element integer vector
         start, end,
         topic,
         ...) {
    b <- list()
    vtypes <- c("archive-retrieval","inventory-retrieval")
    if (!type %in% vtypes) {
        stop("'type' must be one of: ", paste0(vtypes, collapse = ", "))
    }
    b$Type <- type
    if (type == "archive-retrieval") {
        b$ArchiveId <- archive
    }
    if (!missing(description)) {
        b$Description <- description
    }
    if (!missing(marker)) {
        b$Marker <- marker
    }
    if (!missing(format)){
        if (!toupper(format) %in% c("CSV","JSON")) {
            stop("'format' must be 'CSV' or 'JSON'")
        }
        b$Format <- format
    }
    if (!missing(limit)) {
        limit <- as.integer(limit)
        if (limit < 1) {
            stop("'limit' must be >= 1")
        }
        b$Limit <- limit
    }
    if (!missing(bytes)){
        if ((bytes[1] %% 1024) != 0) {
            stop("first element of 'bytes' must be megabyte aligned")
        }
        if (((bytes[1]+1) %% 1024) != 0) {
            stop("second element of bytes +1 must be megabyte aligned")
        }
        b$RetrievalByteRange <- paste(bytes, collapse = "-")
    }
    if (!missing(start)) {
        # need to format correctly
        query$StartTime <- start
    }
    if (!missing(end)) {
        # need to format correctly
        query$EndTime <- end
    }

    
    if (!missing(topic)) {
        b$SNStopic <- topic
    }
    r <- glacierHTTP("POST", paste0("/-/vaults/",vault,"/jobs"), ...)
    return(r)
}

#' @rdname get_job
#' @title Get job
#' @description Get job or job output
#' @template vault
#' @template job
#' @template dots
#' @return A list.
#' @export
get_job <- function(vault, job, ...) {
    r <- glacierHTTP("GET", paste0("/-/vaults/",vault,"/jobs/", job), ...)
    return(r)
}

#' @export
get_job_output <- function(vault, job, ...) {
    r <- glacierHTTP("GET", paste0("/-/vaults/",vault,"/jobs/", job, "/output"), ...)
    return(r)
}

#' @title List jobs
#' @description List jobs
#' @template dots
#' @export
list_jobs <- function(vault, n, completed, marker, status, ...) {
    query <- list()
    if (!missing(completed)) {
        query$completed <- completed
    }
    if (!missing(n)) {
        if(!n %in% 1:1000)
            stop("'n' must be between 1 and 1000")
        query$limit <- n
    }
    if (!missing(marker)) {
        query$marker <- marker
    }
    if (!missing(status)) {
        vstat <- c("InProgress", "Succeeded", "Failed")
        if(!status %in% vstat)
            stop("'status' must be one of: ", paste0(vstat, collapse = ", "))
        query$statuscode <- statuscode
    }
    r <- glacierHTTP("GET", paste0("/-/vaults/",vault,"/jobs"), query = query, ...)
    return(r)
}
