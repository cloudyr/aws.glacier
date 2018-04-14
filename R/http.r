.treehash <- function(body) {
}

#' @title glacierHTTP
#' @description AWS Glacier API Requests
#' @details This is the workhorse function for executing Glacier API requests. It is typically not necessary to call this function directly.
#' @param verb A character string specifying an HTTP method.
#' @param action A character string specifying an API \dQuote{action}.
#' @param query A list of HTTP URL query parameters.
#' @param headers A list of HTTP headers.
#' @param body The body of the request.
#' @param verbose A logical indicating whether to be verbose. Default is given by \code{options("verbose")}.
#' @param region A character string specifying an AWS region. See \code{\link[aws.signature]{locate_credentials}}.
#' @param key A character string specifying an AWS Access Key. See \code{\link[aws.signature]{locate_credentials}}.
#' @param secret A character string specifying an AWS Secret Key. See \code{\link[aws.signature]{locate_credentials}}.
#' @param session_token Optionally, a character string specifying an AWS temporary Session Token to use in signing a request. See \code{\link[aws.signature]{locate_credentials}}.
#' @param \dots Additional arguments passed to \code{\link[httr]{GET}}, etc.
#' @return A list.
#' @import httr
#' @importFrom aws.signature signature_v4_auth
#' @importFrom jsonlite fromJSON
#' @export
glacierHTTP <- 
function(
  verb,
  action,
  query = list(),
  headers = list(),
  body = "", 
  verbose = getOption("verbose", FALSE),
  region = Sys.getenv("AWS_DEFAULT_REGION", "us-east-1"), 
  key = NULL, 
  secret = NULL, 
  session_token = NULL,
  ...
) {
    # locate and validate credentials
    credentials <- locate_credentials(key = key, secret = secret, session_token = session_token, region = region, verbose = verbose)
    key <- credentials[["key"]]
    secret <- credentials[["secret"]]
    session_token <- credentials[["session_token"]]
    region <- credentials[["region"]]
    
    # generate request signature
    if (missing(verb)) {
        verb <- "GET"
    }
    d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    url <- paste0("https://glacier.", region, ".amazonaws.com", action)
    headers$host <- paste0("glacier.",region,".amazonaws.com")
    headers$`x-amz-date` <- d_timestamp
    if (body != "") {
        headers$`x-amz-sha256-tree-hash` <- .treehash(body)
    }
    if (! "x-amz-glacier-version" %in% names(headers)) {
        headers[["x-amz-glacier-version"]] <- "2012-06-01"
    }
    Sig <- signature_v4_auth(
           datetime = d_timestamp,
           region = region,
           service = "glacier",
           verb = verb,
           action = action,
           query_args = query,
           canonical_headers = headers,
           request_body = body,
           key = key, 
           secret = secret,
           session_token = session_token,
           verbose = verbose)
    # setup request headers
    headers[["x-amz-date"]] <- d_timestamp
    headers[["x-amz-content-sha256"]] <- Sig$BodyHash
    headers[["Authorization"]] <- Sig[["SignatureHeader"]]
    if (!is.null(session_token) && session_token != "") {
        headers[["x-amz-security-token"]] <- session_token
    }
    H <- do.call(add_headers, headers)
    
    # execute request
    if (verb == "GET") {
        if (length(query)) {
            r <- GET(url, H, query = query, ...)
        } else {
            r <- GET(url, H, ...)
        }
    } else if (verb == "HEAD") {
        if (length(query)) {
            r <- HEAD(url, H, query = query, ...)
        } else {
            r <- HEAD(url, H, ...)
        }
        return(headers(r))
    } else if (verb == "DELETE") {
        if (length(query)) {
            r <- DELETE(url, H, query = query, ...)
        } else {
            r <- DELETE(url, H, ...)
        }
        return(headers(r))
    } else if (verb == "PUT") {
        if (length(query)) {
            r <- PUT(url, H, query = query, ...)
        } else {
            r <- PUT(url, H, ...)
        }
        return(headers(r))
    } else if (verb == "OPTIONS") {
        if (length(query)) {
            r <- VERB("OPTIONS", url, H, query = query, ...)
        } else {
            r <- VERB("OPTIONS", url, H, ...)
        }
    }
    
    if (http_error(r)) {
        warn_for_status(r)
        h <- headers(r)
        out <- structure(jsonlite::fromJSON(content(r, "text", encoding = "UTF-8")), headers = h, class = "aws_error")
        attr(out, "request_canonical") <- Sig$CanonicalRequest
        attr(out, "request_string_to_sign") <- Sig$StringToSign
        attr(out, "request_signature") <- Sig$SignatureHeader
    } else {
        out <- jsonlite::fromJSON(content(r, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)
    }
    return(out)
}
