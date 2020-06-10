#' Azure resource group class
#'
#' Class representing an Azure resource group.
#'
#' @docType class
#' @section Methods:
#' - `new(token, subscription, id, ...)`: Initialize a resource group object. See 'Initialization' for more details.
#' - `delete(confirm=TRUE)`: Delete this resource group, after a confirmation check. This is asynchronous: while the method returns immediately, the delete operation continues on the host in the background. For resource groups containing a large number of deployed resources, this may take some time to complete.
#' - `sync_fields()`: Synchronise the R object with the resource group it represents in Azure.
#' - `list_templates(filter, top)`: List deployed templates in this resource group. `filter` and `top` are optional arguments to filter the results; see the [Azure documentation](https://docs.microsoft.com/en-us/rest/api/resources/deployments/listbyresourcegroup) for more details. If `top` is specified, the returned list will have a maximum of this many items.
#' - `get_template(name)`: Return an object representing an existing template.
#' - `deploy_template(...)`: Deploy a new template. See 'Templates' for more details. By default, AzureRMR will set the `createdBy` tag on a newly-deployed template to the value `AzureR/AzureRMR`.
#' - `delete_template(name, confirm=TRUE, free_resources=FALSE)`: Delete a deployed template, and optionally free any resources that were created.
#' - `get_resource(...)`: Return an object representing an existing resource. See 'Resources' for more details.
#' - `create_resource(...)`: Create a new resource. By default, AzureRMR will set the `createdBy` tag on a newly-created resource to the value `AzureR/AzureRMR`.
#' - `delete_resource(..., confirm=TRUE, wait=FALSE)`: Delete an existing resource. Optionally wait for the delete to finish.
#' - `resource_exists(...)`: Check if a resource exists.
#' - `list_resources(filter, expand, top)`: Return a list of resource group objects for this subscription. `filter`, `expand` and `top` are optional arguments to filter the results; see the [Azure documentation](https://docs.microsoft.com/en-us/rest/api/resources/resources/list) for more details. If `top` is specified, the returned list will have a maximum of this many items.
#' - `do_operation(...)`: Carry out an operation. See 'Operations' for more details.
#' - `set_tags(..., keep_existing=TRUE)`: Set the tags on this resource group. The tags can be either names or name-value pairs. To delete a tag, set it to `NULL`.
#' - `get_tags()`: Get the tags on this resource group.
#' - `create_lock(name, level)`: Create a management lock on this resource group (which will propagate to all resources within it).
#' - `get_lock(name)`: Returns a management lock object.
#' - `delete_lock(name)`: Deletes a management lock object.
#' - `list_locks()`: List all locks that apply to this resource group. Note this includes locks created at the subscription level, and for any resources within the resource group.
#' - `add_role_assignment(name, ...)`: Adds a new role assignment. See 'Role-based access control' below.
#' - `get_role_assignment(id)`: Retrieves an existing role assignment.
#' - `remove_role_assignment(id)`: Removes an existing role assignment.
#' - `list_role_assignments()`: Lists role assignments.
#' - `get_role_definition(id)`: Retrieves an existing role definition.
#' - `list_role_definitions()` Lists role definitions.
#'
#' @section Initialization:
#' Initializing a new object of this class can either retrieve an existing resource group, or create a new resource group on the host. Generally, the easiest way to create a resource group object is via the `get_resource_group`, `create_resource_group` or `list_resource_groups` methods of the [az_subscription] class, which handle this automatically.
#'
#' To create a resource group object in isolation, supply (at least) an Oauth 2.0 token of class [AzureToken], the subscription ID, and the resource group name. If this object refers to a _new_ resource group, supply the location as well (use the `list_locations` method of the `az_subscription class` for possible locations). You can also pass any optional parameters for the resource group as named arguments to `new()`.
#'
#' @section Templates:
#' To deploy a new template, pass the following arguments to `deploy_template()`:
#' - `name`: The name of the deployment.
#' - `template`: The template to deploy. This can be provided in a number of ways:
#'   1. A nested list of name-value pairs representing the parsed JSON
#'   2. The name of a template file
#'   3. A vector of strings containing unparsed JSON
#'   4. A URL from which the template can be downloaded
#' - `parameters`: The parameters for the template. This can be provided using any of the same methods as the `template` argument.
#' - `wait`: Optionally, whether or not to wait until the deployment is complete before returning. Defaults to `FALSE`.
#'
#' Retrieving or deleting a deployed template requires only the name of the deployment.
#'
#' @section Resources:
#' There are a number of arguments to `get_resource()`, `create_resource()` and `delete_resource()` that serve to identify the specific resource in question:
#' - `id`: The full ID of the resource, including subscription ID and resource group.
#' - `provider`: The provider of the resource, eg `Microsoft.Compute`.
#' - `path`: The full path to the resource, eg `virtualMachines`.
#' - `type`: The combination of provider and path, eg `Microsoft.Compute/virtualMachines`.
#' - `name`: The name of the resource instance, eg `myWindowsVM`.
#'
#' Providing the `id` argument will fill in the values for all the other arguments. Similarly, providing the `type` argument will fill in the values for `provider` and `path`. Unless you provide `id`, you must also provide `name`.
#'
#' To create/deploy a new resource, specify any extra parameters that the provider needs as named arguments to `create_resource()`. Like `deploy_template()`, `create_resource()` also takes an optional `wait` argument that specifies whether to wait until resource creation is complete before returning.
#'
#' @section Operations:
#' The `do_operation()` method allows you to carry out arbitrary operations on the resource group. It takes the following arguments:
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
#' [az_subscription], [az_template], [az_resource],
#' [Azure resource group overview](https://docs.microsoft.com/en-us/azure/azure-resource-manager/resource-group-overview#resource-groups),
#' [Resources API reference](https://docs.microsoft.com/en-us/rest/api/resources/resources),
#' [Template API reference](https://docs.microsoft.com/en-us/rest/api/resources/deployments)
#'
#' For role-based access control methods, see [rbac]
#'
#' For management locks, see [lock]
#'
#' @examples
#' \dontrun{
#'
#' # recommended way to retrieve a resource group object
#' rg <- get_azure_login("myaadtenant")$
#'     get_subscription("subscription_id")$
#'     get_resource_group("rgname")
#'
#' # list resources & templates in this resource group
#' rg$list_resources()
#' rg$list_templates()
#'
#' # get a resource (virtual machine)
#' rg$get_resource(type="Microsoft.Compute/virtualMachines", name="myvm")
#'
#' # create a resource (storage account)
#' rg$create_resource(type="Microsoft.Storage/storageAccounts", name="mystorage",
#'     kind="StorageV2",
#'     sku=list(name="Standard_LRS"))
#'
#' # delete a resource
#' rg$delete_resource(type="Microsoft.Storage/storageAccounts", name="mystorage")
#'
#' # deploy a template
#' rg$deploy_template("tplname",
#'     template="template.json",
#'     parameters="parameters.json")
#'
#' # deploy a template with parameters inline
#' rg$deploy_template("mydeployment",
#'     template="template.json",
#'     parameters=list(parm1="foo", parm2="bar"))
#'
#' # delete a template and free resources
#' rg$delete_template("tplname", free_resources=TRUE)
#'
#' # delete the resource group itself
#' rg$delete()
#'
#' }
#' @format An R6 object of class `az_resource_group`.
#' @export
az_resource_group <- R6::R6Class("az_resource_group",

public=list(
    subscription=NULL,
    id=NULL,
    name=NULL,
    type=NULL,
    location=NULL,
    managed_by=NULL,
    properties=NULL,
    tags=NULL,
    token=NULL,

    # constructor: can refer to an existing RG, or create a new RG
    initialize=function(token, subscription, name=NULL, ..., parms=list())
    {
        if(is_empty(name) && is_empty(parms))
            stop("Must supply either resource group name, or parameter list")

        self$token <- token
        self$subscription <- subscription

        parms <- if(!is_empty(list(...)))
            private$init_and_create(name, ...)
        else private$init(name, parms)

        self$id <- parms$id
        self$type <- parms$type
        self$location <- parms$location
        self$managed_by <- parms$managedBy
        self$properties <- parms$properties
        self$tags <- parms$tags

        NULL
    },

    delete=function(confirm=TRUE)
    {
        if(!delete_confirmed(confirm, self$name, "resource group"))
            return(invisible(NULL))

        private$rg_op(http_verb="DELETE")
        message("Deleting resource group '", self$name, "'. This operation may take some time to complete.")
        invisible(NULL)
    },

    list_templates=function(filter=NULL, top=NULL)
    {
        opts <- list(`$filter`=filter, `$top`=top)
        cont <- private$rg_op("providers/Microsoft.Resources/deployments", options=opts)
        lst <- lapply(
            if(is.null(top))
                get_paged_list(cont, self$token)
            else cont$value,
            function(parms) az_template$new(self$token, self$subscription, self$name, deployed_properties=parms)
        )

        named_list(lst)
    },

    deploy_template=function(name, template, parameters, ...)
    {
        az_template$new(self$token, self$subscription, self$name, name,
                        template, parameters, ...)
    },

    get_template=function(name)
    {
        az_template$new(self$token, self$subscription, self$name, name)
    },

    delete_template=function(name, confirm=TRUE, free_resources=FALSE)
    {
        self$get_template(name)$delete(confirm=confirm, free_resources=free_resources)
    },

    list_resources=function(filter=NULL, expand=NULL, top=NULL)
    {
        opts <- list(`$filter`=filter, `$expand`=expand, `$top`=top)
        cont <- private$rg_op("resources", options=opts)
        lst <- lapply(
            if(is.null(top))
                get_paged_list(cont, self$token)
            else cont$value,
            function(parms) az_resource$new(self$token, self$subscription, deployed_properties=parms)
        )

        names(lst) <- sapply(lst, function(x) sub("^.+providers/(.+$)", "\\1", x$id))
        lst
    },

    get_resource=function(provider, path, type, name, id, api_version=NULL)
    {
        az_resource$new(self$token, self$subscription,
                        resource_group=self$name, provider=provider, path=path, type=type, name=name, id=id,
                        api_version=api_version)
    },

    resource_exists=function(provider, path, type, name, id)
    {
        # HEAD seems to be broken; use GET and check if it succeeds
        res <- try(self$get_resource(provider, path, type, name, id), silent=TRUE)
        !inherits(res, "try-error")
    },

    delete_resource=function(provider, path, type, name, id, api_version=NULL, confirm=TRUE, wait=FALSE)
    {
        # supply deployed_properties arg to prevent querying host for resource info
        az_resource$
            new(self$token, self$subscription, self$name,
                provider=provider, path=path, type=type, name=name, id=id,
                deployed_properties=list(NULL), api_version=api_version)$
            delete(confirm=confirm, wait=wait)
    },

    create_resource=function(provider, path, type, name, id, location=self$location, ...)
    {
        az_resource$new(self$token, self$subscription,
                        resource_group=self$name, provider=provider, path=path, type=type, name=name, id=id,
                        location=location, ...)
    },

    sync_fields=function()
    {
        self$initialize(self$token, self$subscription, name=self$name)
        invisible(NULL)
    },

    set_tags=function(..., keep_existing=TRUE)
    {
        # if tags is uninitialized (NULL), set it to named list
        if(is.null(self$tags))
            self$tags <- named_list()

        tags <- match.call(expand.dots=FALSE)$...
        unvalued <- if(is.null(names(tags)))
            rep(TRUE, length(tags))
        else names(tags) == ""

        values <- lapply(seq_along(unvalued), function(i)
        {
            if(unvalued[i]) "" else as.character(eval.parent(tags[[i]]))
        })
        names(values) <- ifelse(unvalued, as.character(tags), names(tags))

        if(keep_existing)
            values <- modifyList(self$tags, values)

        # delete tags specified to be null
        values <- values[!sapply(values, is_empty)]

        private$rg_op(body=jsonlite::toJSON(list(tags=values), auto_unbox=TRUE, digits=22),
            encode="raw", http_verb="PATCH")
        self$sync_fields()
    },

    get_tags=function()
    {
        if(is.null(self$tags))
            named_list()
        else self$tags
    },

    do_operation=function(..., options=list(), http_verb="GET")
    {
        private$rg_op(..., options=options, http_verb=http_verb)
    },

    print=function(...)
    {
        cat("<Azure resource group ", self$name, ">\n", sep="")
        cat(format_public_fields(self, exclude=c("subscription", "name")))
        cat(format_public_methods(self))
        invisible(self)
    }
),

private=list(

    init=function(name, parms)
    {
        if(is_empty(parms))
        {
            self$name <- name
            parms <- private$rg_op()
        }
        else
        {
            # private$validate_parms(parms)
            self$name <- parms$name
        }
        parms
    },

    init_and_create=function(name, ...)
    {
        parms <- modifyList(list(...), list(name=name))
        parms$tags <- add_creator_tag(parms$tags)
        # private$validate_parms(parms)
        self$name <- name
        private$rg_op(body=parms, encode="json", http_verb="PUT")
    },

    # validate_parms=function(parms)
    # {
    #     required_names <- c("location", "name")
    #     optional_names <- c("id", "managedBy", "tags", "properties", "type")
    #     validate_object_names(names(parms), required_names, optional_names)
    # },

    rg_op=function(op="", ...)
    {
        op <- construct_path("resourcegroups", self$name, op)
        call_azure_rm(self$token, self$subscription, op, ...)
    }
))

