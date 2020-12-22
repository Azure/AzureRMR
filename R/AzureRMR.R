#' @import AzureAuth
#' @import AzureGraph
#' @importFrom utils modifyList
NULL

#' @export
AzureGraph::named_list

#' @export
AzureGraph::is_empty

#' @export
AzureGraph::format_public_fields

#' @export
AzureGraph::format_public_methods

utils::globalVariables(c("self", "private", "pool"))

.onLoad <- function(libname, pkgname)
{
    options(azure_api_version="2019-10-01")
    options(azure_api_mgmt_version="2016-09-01")
    options(azure_roledef_api_version="2018-01-01-preview")
    options(azure_roleasn_api_version="2018-12-01-preview")

    invisible(NULL)
}

.onUnLoad <- function(libname, pkgname)
{
    if(exists("pool", envir=.AzureR, inherits=FALSE))
        try(parallel::stopCluster(.AzureR$pool), silent=TRUE)
}


# default authentication app ID: leverage the az CLI
.az_cli_app_id <- "04b07795-8ddb-461a-bbee-02f9e1bf7b46"

