#' Call the Azure Resource Manager REST API
#'
#' @param token An Azure OAuth token, of class [AzureToken].
#' @param subscription For `call_azure_rm`, a subscription ID.
#' @param operation The operation to perform, which will form part of the URL path.
#' @param options A named list giving the URL query parameters.
#' @param api_version The API version to use, which will form part of the URL sent to the host.
#' @param url A complete URL to send to the host.
#' @param http_verb The HTTP verb as a string, one of `GET`, `PUT`, `POST`, `DELETE`, `HEAD` or `PATCH`.
#' @param http_status_handler How to handle in R the HTTP status code of a response. `"stop"`, `"warn"` or `"message"` will call the appropriate handlers in httr, while `"pass"` ignores the status code.
#' @param auto_refresh Whether to refresh/renew the OAuth token if it is no longer valid.
#' @param body The body of the request, for `PUT`/`POST`/`PATCH`.
#' @param encode The encoding (really content-type) for the request body. The default value "json" means to serialize a list body into a JSON object. If you pass an already-serialized JSON object as the body, set `encode` to "raw".
#' @param ... Other arguments passed to lower-level code, ultimately to the appropriate functions in httr.
#'
#' @details
#' These functions form the low-level interface between R and Azure. `call_azure_rm` builds a URL from its arguments and passes it to `call_azure_url`. Authentication is handled automatically.
#'
#' @return
#' If `http_status_handler` is one of `"stop"`, `"warn"` or `"message"`, the status code of the response is checked. If an error is not thrown, the parsed content of the response is returned with the status code attached as the "status" attribute.
#'
#' If `http_status_handler` is `"pass"`, the entire response is returned without modification.
#'
#' @seealso
#' [httr::GET], [httr::PUT], [httr::POST], [httr::DELETE], [httr::stop_for_status], [httr::content]
#' @rdname call_azure
#' @export
call_azure_rm <- function(token, subscription, operation, ...,
                          options=list(),
                          api_version=getOption("azure_api_version"))
{
    url <- find_resource_host(token)
    url$path <- if(!missing(subscription) && !is.null(subscription))
        construct_path("subscriptions", subscription, operation)
    else operation
    url$query <- modifyList(list(`api-version`=api_version), options)

    call_azure_url(token, url, ...)
}


#' @rdname call_azure
#' @export
call_azure_url <- function(token, url, ..., body=NULL, encode="json",
                           http_verb=c("GET", "DELETE", "PUT", "POST", "HEAD", "PATCH"),
                           http_status_handler=c("stop", "warn", "message", "pass"),
                           auto_refresh=TRUE)
{
    headers <- process_headers(token, url, auto_refresh)

    # if content-type is json, serialize it manually to ensure proper handling of nulls
    if(encode == "json")
    {
        null <- vapply(body, is.null, logical(1))
        body <- jsonlite::toJSON(body[!null], auto_unbox=TRUE, digits=22, null="null")
        encode <- "raw"
    }

    # do actual API call
    res <- httr::VERB(match.arg(http_verb), url, headers, body=body, encode=encode, ...)

    process_response(res, match.arg(http_status_handler))
}


process_headers <- function(token, host, auto_refresh)
{
    # if token has expired, renew it
    if(auto_refresh && !token$validate())
    {
        message("Access token has expired or is no longer valid; refreshing")
        token$refresh()
    }

    access_token <- extract_jwt(token)
    headers <- c(
        Host=httr::parse_url(host)$hostname,
        Authorization=paste("Bearer", access_token),
        `Content-Type`="application/json"
    )

    httr::add_headers(.headers=headers)
}


process_response <- function(response, handler)
{
    if(handler != "pass")
    {
        cont <- httr::content(response)
        handler <- get(paste0(handler, "_for_status"), getNamespace("httr"))
        handler(response, paste0("complete operation. Message:\n",
                                 sub("\\.$", "", error_message(cont))))
        if(is.null(cont))
            cont <- list()
        attr(cont, "status") <- httr::status_code(response)
        cont
    }
    else response
}


# provide complete error messages from Resource Manager
error_message <- function(cont)
{
    # kiboze through possible message locations
    msg <- if(is.character(cont))
        cont
    else if(is.list(cont))
        as.character(unlist(cont))
    else ""

    paste0(strwrap(msg), collapse="\n")
}


find_resource_host <- function(token)
{
    if(is_azure_v2_token(token))
    {
        # search the vector of scopes for the actual resource URL
        url <- list()
        i <- 1
        while(is.null(url$scheme) && i <= length(token$scope))
        {
            url <- httr::parse_url(token$scope[i])
            i <- i + 1
        }
    }
    else url <- httr::parse_url(token$resource) # v1 token is the easy case

    if(is.null(url$scheme))
        stop("Could not find Resource Manager host URL", call.=FALSE)
    url$path <- NULL
    url
}
