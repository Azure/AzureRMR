### base Resource Manager class

#' Azure Resource Manager
#'
#' Base class for interacting with Azure Resource Manager.
#'
#' @docType class
#' @section Methods:
#' - `new(tenant, app, ...)`: Initialize a new ARM connection with the given credentials. See 'Authentication` for more details.
#' - `list_subscriptions()`: Returns a list of objects, one for each subscription associated with this app ID.
#' - `get_subscription(id)`: Returns an object representing a subscription.
#' - `get_subscription_by_name(name)`: Returns the subscription with the given name (as opposed to a GUID).
#' - `do_operation(...)`: Carry out an operation. See 'Operations' for more details.
#'
#' @section Authentication:
#' The recommended way to authenticate with ARM is via the [get_azure_login] function, which creates a new instance of this class.
#'
#' To authenticate with the `az_rm` class directly, provide the following arguments to the `new` method:
#' - `tenant`: Your tenant ID. This can be a name ("myaadtenant"), a fully qualified domain name ("myaadtenant.onmicrosoft.com" or "mycompanyname.com"), or a GUID.
#' - `app`: The client/app ID to use to authenticate with Azure Active Directory. The default is to login interactively using the Azure CLI cross-platform app, but it's recommended to supply your own app credentials if possible.
#' - `password`: if `auth_type == "client_credentials"`, the app secret; if `auth_type == "resource_owner"`, your account password.
#' - `username`: if `auth_type == "resource_owner"`, your username.
#' - `certificate`: If `auth_type == "client_credentials", a certificate to authenticate with. This is a more secure alternative to using an app secret.
#' - `auth_type`: The OAuth authentication method to use, one of "client_credentials", "authorization_code", "device_code" or "resource_owner". See [get_azure_token] for how the default method is chosen, along with some caveats.
#' - `host`: your ARM host. Defaults to `https://management.azure.com/`. Change this if you are using a government or private cloud.
#' - `aad_host`: Azure Active Directory host for authentication. Defaults to `https://login.microsoftonline.com/`. Change this if you are using a government or private cloud.
#' - `...`: Further arguments to pass to `get_azure_token`.
#' - `token`: Optionally, an OAuth 2.0 token, of class [AzureToken]. This allows you to reuse the authentication details for an existing session. If supplied, all other arguments will be ignored.
#'
#' @section Operations:
#' The `do_operation()` method allows you to carry out arbitrary operations on the Resource Manager endpoint. It takes the following arguments:
#' - `op`: The operation in question, which will be appended to the URL path of the request.
#' - `options`: A named list giving the URL query parameters.
#' - `...`: Other named arguments passed to [call_azure_rm], and then to the appropriate call in httr. In particular, use `body` to supply the body of a PUT, POST or PATCH request, and `api_version` to set the API version.
#' - `http_verb`: The HTTP verb as a string, one of `GET`, `PUT`, `POST`, `DELETE`, `HEAD` or `PATCH`.
#'
#' Consult the Azure documentation for what operations are supported.
#'
#' @seealso
#' [create_azure_login], [get_azure_login]
#'
#' [Azure Resource Manager overview](https://docs.microsoft.com/en-us/azure/azure-resource-manager/resource-group-overview),
#' [REST API reference](https://docs.microsoft.com/en-us/rest/api/resources/)
#'
#' @examples
#' \dontrun{
#'
#' # start a new Resource Manager session
#' az <- az_rm$new(tenant="myaadtenant.onmicrosoft.com", app="app_id", password="password")
#'
#' # authenticate with credentials in a file
#' az <- az_rm$new(config_file="creds.json")
#'
#' # authenticate with device code
#' az <- az_rm$new(tenant="myaadtenant.onmicrosoft.com", app="app_id", auth_type="device_code")
#'
#' # retrieve a list of subscription objects
#' az$list_subscriptions()
#'
#' # a specific subscription
#' az$get_subscription("subscription_id")
#'
#' }
#' @format An R6 object of class `az_rm`.
#' @export
az_rm <- R6::R6Class("az_rm",

public=list(
    host=NULL,
    tenant=NULL,
    token=NULL,

    # authenticate and get subscriptions
    initialize=function(tenant="common", app=.az_cli_app_id,
                        password=NULL, username=NULL, certificate=NULL, auth_type=NULL,
                        host="https://management.azure.com/", aad_host="https://login.microsoftonline.com/",
                        token=NULL, ...)
    {
        if(is_azure_token(token))
        {
            self$host <- httr::build_url(find_resource_host(token))
            self$tenant <- token$tenant
            self$token <- token
            return(NULL)
        }

        self$host <- host
        self$tenant <- normalize_tenant(tenant)
        app <- normalize_guid(app)

        token_args <- list(resource=self$host,
            tenant=self$tenant,
            app=app,
            password=password,
            username=username,
            certificate=certificate,
            auth_type=auth_type,
            aad_host=aad_host,
            ...)

        self$token <- do.call(get_azure_token, token_args)
        NULL
    },

    # return a subscription object
    get_subscription=function(id)
    {
        az_subscription$new(self$token, id)
    },

    # return a subscription object given its name
    get_subscription_by_name=function(name)
    {
        subs <- self$list_subscriptions()
        found <- which(sapply(subs, function(x) x$name) == name)
        if(is_empty(found))
            stop("Subscription '", name, "' not found", call.=FALSE)
        if(length(found) > 1)
            stop("More than 1 subscription with the name '", name, "'", call.=FALSE) # sanity check
        subs[[found]]
    },

    # return all subscriptions for this app
    list_subscriptions=function()
    {
        cont <- call_azure_rm(self$token, subscription="", operation="")
        lst <- lapply(get_paged_list(cont, self$token), function(parms)
            az_subscription$new(self$token, parms=parms))
        named_list(lst, "id")
    },

    do_operation=function(..., options=list(), http_verb="GET")
    {
        private$rm_op(..., options=options, http_verb=http_verb)
    },

    print=function(...)
    {
        cat("<Azure Resource Manager client>\n")
        cat("<Authentication>\n")
        fmt_token <- gsub("\n  ", "\n    ", format_auth_header(self$token))
        cat(" ", fmt_token)
        cat("---\n")
        cat(format_public_methods(self))
        invisible(self)
    }
),

private=list(

    rm_op=function(op="", options=list(), ...)
    {
        call_azure_rm(self$token, subscription=NULL, op, ...)
    }
))

