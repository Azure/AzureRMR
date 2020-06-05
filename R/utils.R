#' Miscellaneous utility functions
#'
#' @param lst A named list of objects.
#' @param name_fields The components of the objects in `lst`, to be used as names.
#' @param x For `is_url` and `is_empty`, An R object.
#' @param https_only For `is_url`, whether to allow only HTTPS URLs.
#' @param token For `get_paged_list`, an Azure OAuth token, of class [AzureToken].
#' @param next_link_name,value_name For `get_paged_list`, the names of the next link and value components in the `lst` argument. The default values are correct for Resource Manager.
#'
#' @details
#' `named_list` extracts from each object in `lst`, the components named by `name_fields`. It then constructs names for `lst` from these components, separated by a `"/"`.
#'
#' `get_paged_list` reconstructs a complete list of objects from a paged response. Many Resource Manager list operations will return _paged_ output, that is, the response contains a subset of all items, along with a URL to query to retrieve the next subset. `get_paged_list` retrieves each subset and returns all items in a single list.
#'
#' @return
#' For `named_list`, the list that was passed in but with names. An empty input results in a _named list_ output: a list of length 0, with a `names` attribute.
#'
#' For `get_paged_list`, a list.
#'
#' For `is_url`, whether the object appears to be a URL (is character of length 1, and starts with the string `"http"`). Optionally, restricts the check to HTTPS URLs only. For `is_empty`, whether the length of the object is zero (this includes the special case of `NULL`).
#'
#' @rdname utils
#' @export
named_list <- function(lst=NULL, name_fields="name")
{
    if(is_empty(lst))
        return(structure(list(), names=character(0)))

    lst_names <- sapply(name_fields, function(n) sapply(lst, `[[`, n))
    if(length(name_fields) > 1)
    {
        dim(lst_names) <- c(length(lst_names) / length(name_fields), length(name_fields))
        lst_names <- apply(lst_names, 1, function(nn) paste(nn, collapse="/"))
    }
    names(lst) <- lst_names
    dups <- duplicated(tolower(names(lst)))
    if(any(dups))
    {
        duped_names <- names(lst)[dups]
        warning("Some names are duplicated: ", paste(unique(duped_names), collapse=" "), call.=FALSE)
    }
    lst
}


# check if a string appears to be a http/https URL, optionally only https allowed
#' @rdname utils
#' @export
is_url <- function(x, https_only=FALSE)
{
    pat <- if(https_only) "^https://" else "^https?://"
    is.character(x) && length(x) == 1 && grepl(pat, x)
}


# TRUE for NULL and length-0 objects
#' @rdname utils
#' @export
is_empty <- function(x)
{
    length(x) == 0
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
