#' Login to Azure Resource Manager
#'
#' @param tenant The Azure Active Directory tenant for which to obtain a login client. Can be a name ("myaadtenant"), a fully qualified domain name ("myaadtenant.onmicrosoft.com" or "mycompanyname.com"), or a GUID. The default is to login via the "common" tenant, which will infer your actual tenant from your credentials.
#' @param app The client/app ID to use to authenticate with Azure Active Directory. The default is to login interactively using the Azure CLI cross-platform app, but you can supply your own app credentials as well.
#' @param password If `auth_type == "client_credentials"`, the app secret; if `auth_type == "resource_owner"`, your account password.
#' @param username If `auth_type == "resource_owner"`, your username.
#' @param certificate If `auth_type == "client_credentials", a certificate to authenticate with. This is a more secure alternative to using an app secret.
#' @param auth_type The OAuth authentication method to use, one of "client_credentials", "authorization_code", "device_code" or "resource_owner". If `NULL`, this is chosen based on the presence of the `username` and `password` arguments.
#' @param host Your ARM host. Defaults to `https://management.azure.com/`. Change this if you are using a government or private cloud.
#' @param aad_host Azure Active Directory host for authentication. Defaults to `https://login.microsoftonline.com/`. Change this if you are using a government or private cloud.
#' @param config_file Optionally, a JSON file containing any of the arguments listed above. Arguments supplied in this file take priority over those supplied on the command line. You can also use the output from the Azure CLI `az ad sp create-for-rbac` command.
#' @param token Optionally, an OAuth 2.0 token, of class [AzureToken]. This allows you to reuse the authentication details for an existing session. If supplied, the other arguments above to `create_azure_login` will be ignored.
#' @param graph_host The Microsoft Graph endpoint. See 'Microsoft Graph integration' below.
#' @param refresh For `get_azure_login`, whether to refresh the authentication token on loading the client.
#' @param selection For `get_azure_login`, if you have multiple logins for a given tenant, which one to use. This can be a number, or the input MD5 hash of the token used for the login. If not supplied, `get_azure_login` will print a menu and ask you to choose a login.
#' @param confirm For `delete_azure_login`, whether to ask for confirmation before deleting.
#' @param ... For `create_azure_login`, other arguments passed to `get_azure_token`.
#'
#' @details
#' `create_azure_login` creates a login client to authenticate with Azure Resource Manager (ARM), using the supplied arguments. The Azure Active Directory (AAD) authentication token is obtained using [get_azure_token], which automatically caches and reuses tokens for subsequent sessions. Note that credentials are only cached if you allowed AzureRMR to create a data directory at package startup.
#'
#' `create_azure_login()` without any arguments is roughly equivalent to the Azure CLI command `az login`.
#'
#' `get_azure_login` returns a login client by retrieving previously saved credentials. It searches for saved credentials according to the supplied tenant; if multiple logins are found, it will prompt for you to choose one.
#'
#' One difference between `create_azure_login` and `get_azure_login` is the former will delete any previously saved credentials that match the arguments it was given. You can use this to force AzureRMR to remove obsolete tokens that may be lying around.
#'
#' @section Microsoft Graph integration:
#' If the AzureGraph package is installed and the `graph_host` argument is not `NULL`, `create_azure_login` will also create a login client for Microsoft Graph with the same credentials. This is to facilitate working with registered apps and service principals, eg when managing roles and permissions. Some Azure services also require creating service principals as part of creating a resource (eg Azure Kubernetes Service), and keeping the Graph credentials consistent with ARM helps ensure nothing breaks.
#'
#' @section Linux DSVM note:
#' If you are using a Linux [Data Science Virtual Machine](https://azure.microsoft.com/en-us/services/virtual-machines/data-science-virtual-machines/) in Azure, you may have problems running `create_azure_login()` (ie, without any arguments). In this case, try `create_azure_login(auth_type="device_code")`.
#'
#' @return
#' For `get_azure_login` and `create_azure_login`, an object of class `az_rm`, representing the ARM login client. For `list_azure_logins`, a (possibly nested) list of such objects.
#'
#' If the AzureRMR data directory for saving credentials does not exist, `get_azure_login` will throw an error.
#'
#' @seealso
#' [az_rm], [AzureAuth::get_azure_token] for more details on authentication methods, [AzureGraph::create_graph_login] for the corresponding function to create a Microsoft Graph login client
#'
#' [Azure Resource Manager overview](https://docs.microsoft.com/en-us/azure/azure-resource-manager/resource-group-overview),
#' [REST API reference](https://docs.microsoft.com/en-us/rest/api/resources/)
#'
#' [Authentication in Azure Active Directory](https://docs.microsoft.com/en-us/azure/active-directory/develop/authentication-scenarios)
#'
#' [Azure CLI documentation](https://docs.microsoft.com/en-us/cli/azure/?view=azure-cli-latest)
#'
#' @examples
#' \dontrun{
#'
#' # without any arguments, this will create a client using your AAD credentials
#' az <- create_azure_login()
#'
#' # retrieve the login in subsequent sessions
#' az <- get_azure_login()
#'
#' # this will create a Resource Manager client for the AAD tenant 'myaadtenant.onmicrosoft.com',
#' # using the client_credentials method
#' az <- create_azure_login("myaadtenant", app="app_id", password="password")
#'
#' # you can also login using credentials in a json file
#' az <- create_azure_login(config_file="~/creds.json")
#'
#' }
#' @rdname azure_login
#' @export
create_azure_login <- function(tenant="common", app=.az_cli_app_id,
                               password=NULL, username=NULL, certificate=NULL, auth_type=NULL,
                               host="https://management.azure.com/", aad_host="https://login.microsoftonline.com/",
                               config_file=NULL, token=NULL, graph_host="https://graph.microsoft.com/", ...)
{
    if(!is_azure_token(token))
    {
        if(!is.null(config_file))
        {
            conf <- jsonlite::fromJSON(config_file)
            call <- as.list(match.call())[-1]
            call$config_file <- NULL
            call <- lapply(modifyList(call, conf), function(x) eval.parent(x))
            return(do.call(create_azure_login, call))
        }

        tenant <- normalize_tenant(tenant)
        app <- normalize_guid(app)

        token_args <- list(resource=host,
            tenant=tenant,
            app=app,
            password=password,
            username=username,
            certificate=certificate,
            auth_type=auth_type,
            aad_host=aad_host,
            ...)

        hash <- do.call(token_hash, token_args)
        tokenfile <- file.path(AzureR_dir(), hash)
        if(file.exists(tokenfile))
        {
            message("Deleting existing Azure Active Directory token for this set of credentials")
            file.remove(tokenfile)
        }

        message("Creating Azure Resource Manager login for ", format_tenant(tenant))
        token <- do.call(get_azure_token, token_args)
    }
    else tenant <- token$tenant

    client <- az_rm$new(token=token)

    # save login info for future sessions
    arm_logins <- load_arm_logins()
    arm_logins[[tenant]] <- sort(unique(c(arm_logins[[tenant]], client$token$hash())))
    save_arm_logins(arm_logins)

    make_graph_login_from_token(token, aad_host, graph_host)

    client
}


#' @rdname azure_login
#' @export
get_azure_login <- function(tenant="common", selection=NULL, refresh=TRUE)
{
    if(!dir.exists(AzureR_dir()))
        stop("AzureR data directory does not exist; cannot load saved logins")

    tenant <- normalize_tenant(tenant)

    arm_logins <- load_arm_logins()
    this_login <- arm_logins[[tenant]]
    if(is_empty(this_login))
    {
        msg <- paste0("No Azure Resource Manager logins found for ", format_tenant(tenant),
                      ";\nuse create_azure_login() to create one")
        stop(msg, call.=FALSE)
    }

    if(length(this_login) == 1 && is.null(selection))
        selection <- 1
    else if(is.null(selection))
    {
        tokens <- lapply(this_login, function(f)
            readRDS(file.path(AzureR_dir(), f)))

        choices <- sapply(tokens, function(token)
        {
            app <- token$client$client_id
            paste0("App ID: ", app, "\n   Authentication method: ", token$auth_type)
        })

        msg <- paste0("Choose an Azure Resource Manager login for ", format_tenant(tenant))
        selection <- utils::menu(choices, title=msg)
    }

    if(selection == 0)
        return(NULL)

    file <- if(is.numeric(selection))
        this_login[selection]
    else if(is.character(selection))
        this_login[which(this_login == selection)] # force an error if supplied hash doesn't match available logins

    file <- file.path(AzureR_dir(), file)
    if(is_empty(file) || !file.exists(file))
        stop("Azure Active Directory token not found for this login", call.=FALSE)

    message("Loading Azure Resource Manager login for ", format_tenant(tenant))

    token <- readRDS(file)
    client <- az_rm$new(token=token)

    if(refresh)
        client$token$refresh()

    client
}


#' @rdname azure_login
#' @export
delete_azure_login <- function(tenant="common", confirm=TRUE)
{
    if(!dir.exists(AzureR_dir()))
    {
        warning("AzureR data directory does not exist; no logins to delete")
        return(invisible(NULL))
    }

    tenant <- normalize_tenant(tenant)

    if(!delete_confirmed(confirm, format_tenant(tenant), "Azure Resource Manager login(s) for", FALSE))
        return(invisible(NULL))

    arm_logins <- load_arm_logins()
    arm_logins[[tenant]] <- NULL
    save_arm_logins(arm_logins)
    invisible(NULL)
}


#' @rdname azure_login
#' @export
list_azure_logins <- function()
{
    arm_logins <- load_arm_logins()
    logins <- sapply(arm_logins, function(tenant)
    {
        sapply(tenant, function(hash)
        {
            file <- file.path(AzureR_dir(), hash)
            az_rm$new(token=readRDS(file))
        }, simplify=FALSE)
    }, simplify=FALSE)

    logins
}


load_arm_logins <- function()
{
    file <- file.path(AzureR_dir(), "arm_logins.json")
    if(!file.exists(file))
        return(named_list())
    jsonlite::fromJSON(file)
}


save_arm_logins <- function(logins)
{
    if(!dir.exists(AzureR_dir()))
    {
        message("AzureR data directory does not exist; login credentials not saved")
        return(invisible(NULL))
    }

    if(is_empty(logins))
        names(logins) <- character(0)

    file <- file.path(AzureR_dir(), "arm_logins.json")
    writeLines(jsonlite::toJSON(logins, auto_unbox=TRUE, pretty=TRUE), file)
    invisible(NULL)
}


format_tenant <- function(tenant)
{
    if(tenant == "common")
        "default tenant"
    else paste0("tenant '", tenant, "'")
}