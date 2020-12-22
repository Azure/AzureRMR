#' Miscellaneous utility functions
#'
#' @param lst A named list of objects.
#' @param x For `is_url`, An R object.
#' @param https_only For `is_url`, whether to allow only HTTPS URLs.
#' @param token For `get_paged_list`, an Azure OAuth token, of class [AzureToken].
#' @param next_link_name,value_name For `get_paged_list`, the names of the next link and value components in the `lst` argument. The default values are correct for Resource Manager.
#'
#' @details
#' `get_paged_list` reconstructs a complete list of objects from a paged response. Many Resource Manager list operations will return _paged_ output, that is, the response contains a subset of all items, along with a URL to query to retrieve the next subset. `get_paged_list` retrieves each subset and returns all items in a single list.
#'
#' @return
#' For `get_paged_list`, a list.
#'
#' For `is_url`, whether the object appears to be a URL (is character of length 1, and starts with the string `"http"`). Optionally, restricts the check to HTTPS URLs only.
#'
#' @rdname utils
#' @export
is_url <- function(x, https_only=FALSE)
{
    pat <- if(https_only) "^https://" else "^https?://"
    is.character(x) && length(x) == 1 && grepl(pat, x)
}


# combine several pages of objects into a single list
#' @rdname utils
#' @export
get_paged_list <- function(lst, token, next_link_name="nextLink", value_name="value")
{
    res <- lst[[value_name]]
    while(!is_empty(lst[[next_link_name]]))
    {
        lst <- call_azure_url(token, lst[[next_link_name]])
        res <- c(res, lst[[value_name]])
    }
    res
}


# check that 1) all required names are present; 2) optional names may be present; 3) no other names are present
# validate_object_names <- function(x, required, optional=character(0))
# {
#     valid <- all(required %in% x) && all(x %in% c(required, optional))
#     if(!valid)
#         stop("Invalid object names")
# }


# handle different behaviour of file_path on Windows/Linux wrt trailing /
construct_path <- function(...)
{
    sub("/$", "", file.path(..., fsep="/"))
}


# TRUE if delete confirmed, FALSE otherwise
delete_confirmed <- function(confirm, name, type, quote_name=TRUE)
{
    if(!interactive() || !confirm)
        return(TRUE)

    msg <- if(quote_name)
        sprintf("Do you really want to delete the %s '%s'?", type, name)
    else sprintf("Do you really want to delete the %s %s?", type, name)
    ok <- if(getRversion() < numeric_version("3.5.0"))
    {
        msg <- paste(msg, "(yes/No/cancel) ")
        yn <- readline(msg)
        if(nchar(yn) == 0)
            FALSE
        else tolower(substr(yn, 1, 1)) == "y"
    }
    else utils::askYesNo(msg, FALSE)
    isTRUE(ok)
}


# add a tag on objects created by this package
add_creator_tag <- function(tags)
{
    if(!is.list(tags))
        tags <- list()
    utils::modifyList(list(createdBy="AzureR/AzureRMR"), tags)
}
