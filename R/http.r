.treehash <- function(body) {
}

glacierHTTP <- function(verb, action, query = list(), headers = list(), body = "", 
                        region = Sys.getenv("AWS_DEFAULT_REGION","us-east-1"), 
                        key = Sys.getenv("AWS_ACCESS_KEY_ID"), 
                        secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"), 
                        ...) {
    if(missing(verb))
        verb <- "GET"
    current <- Sys.time()
    d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
    url <- paste0("https://glacier.", region, ".amazonaws.com", action)
    if(key == "") {
        stop("Must supply AWS AccessKeyId and SecretAccessKey")
    } else {
        headers$host <- paste0("glacier.",region,".amazonaws.com")
        headers$`x-amz-date` <- d_timestamp
        if(body != "") {
            headers$`x-amz-sha256-tree-hash` <- .treehash(body)
        }
        S <- signature_v4_auth(
               datetime = d_timestamp,
               region = region,
               service = "glacier",
               verb = verb,
               action = action,
               query_args = query,
               canonical_headers = headers,
               request_body = body,
               key = key, secret = secret)
        headers$`x-amz-content-sha256` <- S$BodyHash
        headers$Authorization <- S$SignatureHeader
        H <- do.call("add_headers", headers)
    }
    if(verb == "GET") {
        if(length(query))
            r <- GET(url, H, query = query, ...)
        else
            r <- GET(url, H, ...)
    } else if (verb == "HEAD") {
        if(length(query))
            r <- HEAD(url, H, query = query, ...)
        else
            r <- HEAD(url, H, ...)
        return(headers(r))
    } else if (verb == "DELETE") {
        if(length(query))
            r <- DELETE(url, H, query = query, ...)
        else
            r <- DELETE(url, H, ...)
        return(headers(r))
    } else if (verb == "PUT") {
        if(length(query))
            r <- PUT(url, H, query = query, ...)
        else
            r <- PUT(url, H, ...)
        return(headers(r))
    } else if (verb == "OPTIONS") {
        if(length(query))
            r <- VERB("OPTIONS", url, H, query = query, ...)
        else
            r <- VERB("OPTIONS", url, H, ...)
    }
    
    x <- xmlToList(xmlParse(content(r, "text")))
    if(http_status(r)$category == "client error") {
        warn_for_status(r)
        h <- headers(r)
        out <- structure(x, headers = h, class = "aws_error")
    } else {
        out <- x
    }
    if(inherits(out, "aws_error")) {
        attr(out, "request_canonical") <- S$CanonicalRequest
        attr(out, "request_string_to_sign") <- S$StringToSign
        attr(out, "request_signature") <- S$SignatureHeader
    }
    return(out)
}
