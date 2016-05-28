#' @title Initiate job
#' @description Initiate a Glacier job
#' @template vault
#' @param description Optionally, a character string specifying a description of the job.
#' @param type A character string specifying the job type. Must be either \dQuote{archive-retrieval} or \dQuote{inventory-retrieval}.
#' @param archive When \code{type = "archive-retrieval"}, an AWS Glacier archive ID, possibly returned by \code{\link{upload_archive}}.
#' @param format Optionally, a character string specifying the format of the job output. Must be one of either \dQuote{CSV} or \dQuote{JSON}.
#' @param limit Optionally, an integer specifying the maximum number of inventory items returned per vault inventory retrieval request.
#' @template marker
#' @param bytes Optionally, either a character string containing a hyphenated byte range to retrieve (e.g., \code{"1-10"}). This can also be specified as a two-element integer vector specifying the start and end of the range.
#' @param start Optionally, an character string containing an ISO 8601 datetime specifying the start of the date range for vault inventory retrieval that includes archives created after this date.
#' @param end Optionally, an character string containing an ISO 8601 datetime specifying the end of the date range for vault inventory retrieval that includes archives created before this date.
#' @template topic
#' @template dots
#' @export
initiate_job <- 
function(vault, 
         description,
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
    vtypes <- c("archive-retrieval", "inventory-retrieval")
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
        if (is.character(bytes)) {
            b$RetrievalByteRange <- bytes
        } else {
            if ((bytes[1] %% 1024) != 0) {
                stop("first element of 'bytes' must be megabyte aligned")
            }
            if (((bytes[1]+1) %% 1024) != 0) {
                stop("second element of bytes +1 must be megabyte aligned")
            }
            b$RetrievalByteRange <- paste(bytes, collapse = "-")
        }
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

#' @rdname get_job
#' @export
get_job_output <- function(vault, job, ...) {
    r <- glacierHTTP("GET", paste0("/-/vaults/",vault,"/jobs/", job, "/output"), ...)
    return(r)
}

#' @title List jobs
#' @description List jobs
#' @template vault
#' @param n An integer specifying the number of jobs to return. The default (and maximum) is 1000.
#' @param completed Optionally, a logical specifying whether to return only jobs that are completed (\code{TRUE}) or not (\code{FALSE}).
#' @template marker
#' @param status Optionally, a character string specifying the completion status of the jobs to return. If specified, must be one of: \dQuote{InProgress}, \dQuote{Succeeded}, or \dQuote{Failed}.
#' @template dots
#' @export
list_jobs <- function(vault, n = 1000L, completed, marker, status, ...) {
    query <- list()
    if (!missing(completed)) {
        query$completed <- completed
    }
    if (!missing(n)) {
        if (!n %in% 1:1000) {
            stop("'n' must be between 1 and 1000")
        }
        query$limit <- n
    }
    if (!missing(marker)) {
        query$marker <- marker
    }
    if (!missing(status)) {
        vstat <- c("InProgress", "Succeeded", "Failed")
        if (!status %in% vstat) {
            stop("'status' must be one of: ", paste0(vstat, collapse = ", "))
        }
        query$statuscode <- status
    }
    r <- glacierHTTP("GET", paste0("/-/vaults/",vault,"/jobs"), query = query, ...)
    return(r)
}
