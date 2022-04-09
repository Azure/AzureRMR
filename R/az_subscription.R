### Azure subscription class: all info about a subscription

#' Azure subscription class
#'
#' Class representing an Azure subscription.
#'
#' @docType class
#' @section Methods:
#' - `new(token, id, ...)`: Initialize a subscription object.
#' - `list_resource_groups(filter, top)`: Return a list of resource group objects for this subscription. `filter` and `top` are optional arguments to filter the results; see the [Azure documentation](https://docs.microsoft.com/en-us/rest/api/resources/resourcegroups/list) for more details. If `top` is specified, the returned list will have a maximum of this many items.
#' - `get_resource_group(name)`: Return an object representing an existing resource group.
#' - `create_resource_group(name, location)`: Create a new resource group in the specified region/location, and return an object representing it. By default, AzureRMR will set the `createdBy` tag on a newly-created resource group to the value `AzureR/AzureRMR`.
#' - `delete_resource_group(name, confirm=TRUE)`: Delete a resource group, after asking for confirmation.
#' - `resource_group_exists(name)`: Check if a resource group exists.
#' - `list_resources(filter, expand, top)`: List all resources deployed under this subscription. `filter`, `expand` and `top` are optional arguments to filter the results; see the [Azure documentation](https://docs.microsoft.com/en-us/rest/api/resources/resources/list) for more details. If `top` is specified, the returned list will have a maximum of this many items.
#' - `list_locations(info=c("partial", "all"))`: List locations available. The default `info="partial"` returns a subset of the information about each location; set `info="all"` to return everything.
#' - `get_provider_api_version(provider, type, which=1, stable_only=TRUE)`: Get the current API version for the given resource provider and type. If no resource type is supplied, returns a vector of API versions, one for each resource type for the given provider. If neither provider nor type is supplied, returns the API versions for all resources and providers. Set `stable_only=FALSE` to allow preview APIs to be returned. Set `which` to a number > 1 to return an API other than the most recent.
#' - `do_operation(...)`: Carry out an operation. See 'Operations' for more details.
#' - `create_lock(name, level)`: Create a management lock on this subscription (which will propagate to all resources within it).
#' - `get_lock(name)`: Returns a management lock object.
#' - `delete_lock(name)`: Deletes a management lock object.
#' - `list_locks()`: List all locks that exist in this subscription.
#' - `add_role_assignment(name, ...)`: Adds a new role assignment. See 'Role-based access control' below.
#' - `get_role_assignment(id)`: Retrieves an existing role assignment.
#' - `remove_role_assignment(id)`: Removes an existing role assignment.
#' - `list_role_assignments()`: Lists role assignments.
#' - `get_role_definition(id)`: Retrieves an existing role definition.
#' - `list_role_definitions()` Lists role definitions.
#' - `get_tags()` Get the tags on this subscription.
#'
#' @section Details:
#' Generally, the easiest way to create a subscription object is via the `get_subscription` or `list_subscriptions` methods of the [az_rm] class. To create a subscription object in isolation, call the `new()` method and supply an Oauth 2.0 token of class [AzureToken], along with the ID of the subscription.
#'
#' @section Operations:
#' The `do_operation()` method allows you to carry out arbitrary operations on the subscription. It takes the following arguments:
#' - `op`: The operation in question, which will be appended to the URL path of the request.
#' - `options`: A named list giving the URL query parameters.
#' - `...`: Other named arguments passed to [call_azure_rm], and then to the appropriate call in httr. In particular, use `body` to supply the body of a PUT, POST or PATCH request, and `api_version` to set the API version.
#' - `http_verb`: The HTTP verb as a string, one of `GET`, `PUT`, `POST`, `DELETE`, `HEAD` or `PATCH`.
#'
#' Consult the Azure documentation for what operations are supported.
#'
#' @section Role-based access control:
#' AzureRMR implements a subset of the full RBAC functionality within Azure Active Directory. You can retrieve role definitions and add and remove role assignments, at the subscription, resource group and resource levels. See [rbac] for more information.
#'
#' @seealso
#' [Azure Resource Manager overview](https://docs.microsoft.com/en-us/azure/azure-resource-manager/resource-group-overview)
#'
#' For role-based access control methods, see [rbac]
#'
#' For management locks, see [lock]
#'
#' @examples
#' \dontrun{
#'
#' # recommended way to retrieve a subscription object
#' sub <- get_azure_login("myaadtenant")$
#'     get_subscription("subscription_id")
#'
#' # retrieve list of resource group objects under this subscription
#' sub$list_resource_groups()
#'
#' # get a resource group
#' sub$get_resource_group("rgname")
#'
#' # check if a resource group exists, and if not, create it
#' rg_exists <- sub$resource_group_exists("rgname")
#' if(!rg_exists)
#'     sub$create_resource_group("rgname", location="australiaeast")
#'
#' # delete a resource group
#' sub$delete_resource_group("rgname")
#'
#' # get provider API versions for some resource types
#' sub$get_provider_api_version("Microsoft.Compute", "virtualMachines")
#' sub$get_provider_api_version("Microsoft.Storage", "storageAccounts")
#'
#' }
#' @format An R6 object of class `az_subscription`.
#' @export
az_subscription <- R6::R6Class("az_subscription",

public=list(
    id=NULL,
    name=NULL,
    state=NULL,
    policies=NULL,
    authorization_source=NULL,
    tags=NULL,
    token=NULL,

    initialize=function(token, id=NULL, parms=list())
    {
        if(is_empty(id) && is_empty(parms))
            stop("Must supply either subscription ID, or parameter list")

        self$token <- token

        if(is_empty(parms))
            parms <- call_azure_rm(self$token, subscription=id, operation="")

        self$id <- parms$subscriptionId
        self$name <- parms$displayName
        self$state <- parms$state
        self$policies <- parms$subscriptionPolicies
        self$authorization_source <- parms$authorizationSource
        self$tags <- parms$tags
        NULL
    },

    list_locations=function(info=c("partial", "all"))
    {
        info <- match.arg(info)
        res <- self$do_operation("locations", http_status_handler="pass")
        cont <- httr::content(res, simplifyVector=TRUE)
        httr::stop_for_status(res, paste0("complete operation. Message:\n",
            sub("\\.$", "", error_message(cont))))

        locs <- cont$value
        locs$metadata$longitude <- as.numeric(locs$metadata$longitude)
        locs$metadata$latitude <- as.numeric(locs$metadata$latitude)
        if(info == "partial")
            cbind(locs[c("name", "displayName")], locs$metadata[c("longitude", "latitude", "regionType")])
        else locs
    },

    # API versions vary across different providers; find the latest
    get_provider_api_version=function(provider=NULL, type=NULL, which=1, stable_only=TRUE)
    {
        select_version <- function(api)
        {
            versions <- unlist(api$apiVersions)
            if(stable_only)
                versions <- grep("preview", versions, value=TRUE, invert=TRUE)

            if(length(versions) >= which) versions[which] else ""
        }

        if(is_empty(provider))
        {
            apis <- named_list(private$sub_op("providers")$value, "namespace")
            lapply(apis, function(api)
            {
                api <- named_list(api$resourceTypes, "resourceType")
                sapply(api, select_version)
            })
        }
        else
        {
            op <- construct_path("providers", provider)
            apis <- named_list(private$sub_op(op)$resourceTypes, "resourceType")
            if(!is_empty(type))
            {
                # case-insensitive matching
                names(apis) <- tolower(names(apis))
                select_version(apis[[tolower(type)]])
            }
            else sapply(apis, select_version)
        }
    },
    
    get_tags=function()
    {
        if(is.null(self$tags))
            named_list()
        else self$tags
    },

    get_resource_group=function(name)
    {
        az_resource_group$new(self$token, self$id, name)
    },

    list_resource_groups=function(filter=NULL, top=NULL)
    {
        opts <- list(`$filter`=filter, `$top`=top)
        cont <- private$sub_op("resourcegroups", options=opts)
        lst <- lapply(
            if(is.null(top))
                get_paged_list(cont, self$token)
            else cont$value,
            function(parms) az_resource_group$new(self$token, self$id, parms=parms)
        )

        named_list(lst)
    },

    create_resource_group=function(name, location, ...)
    {
        az_resource_group$new(self$token, self$id, name, location=location, ...)
    },

    delete_resource_group=function(name, confirm=TRUE)
    {
        if(name == "")
            stop("Must supply a resource group name", call.=FALSE)
        self$get_resource_group(name)$delete(confirm=confirm)
    },

    resource_group_exists=function(name)
    {
        res <- private$sub_op(construct_path("resourceGroups", name),
            http_verb="HEAD", http_status_handler="pass")
        httr::status_code(res) < 300
    },

    list_resources=function(filter=NULL, expand=NULL, top=NULL)
    {
        opts <- list(`$filter`=filter, `$expand`=expand, `$top`=top)
        cont <- private$sub_op("resources", options=opts)
        lst <- lapply(
            if(is.null(top))
                get_paged_list(cont, self$token)
            else cont$value,
            function(parms) az_resource$new(self$token, self$id, deployed_properties=parms)
        )

        names(lst) <- sapply(lst, function(x) sub("^.+providers/(.+$)", "\\1", x$id))
        lst
    },

    do_operation=function(..., options=list(), http_verb="GET")
    {
        private$sub_op(..., options=options, http_verb=http_verb)
    },

    print=function(...)
    {
        cat("<Azure subscription ", self$id, ">\n", sep="")
        cat(format_public_fields(self, exclude="id"))
        cat(format_public_methods(self))
        invisible(self)
    }
),

private=list(

    sub_op=function(op="", ...)
    {
        call_azure_rm(self$token, self$id, op, ...)
    }
))
