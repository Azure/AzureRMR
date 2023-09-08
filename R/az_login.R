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
#' @param version The Azure Active Directory version to use for authenticating.
#' @param scopes The Azure Service Management scopes (permissions) to obtain for this login. Only for `version=2`.
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
                               password=NULL, username=NULL, certificate=NULL, auth_type=NULL, version=2,
                               host="https://management.azure.com/", aad_host="https://login.microsoftonline.com/",
                               scopes=".default", config_file=NULL, token=NULL,
                               graph_host="https://graph.microsoft.com/", ...)
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

        newhost <- if(version == 2)
            c(paste0(host, scopes), "openid", "offline_access")
        else host

        token_args <- list(resource=newhost,
            tenant=tenant,
            app=app,
            password=password,
            username=username,
            certificate=certificate,
            auth_type=auth_type,
            aad_host=aad_host,
            version=version,
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

    make_graph_login_from_token(token, host, graph_host)

    client
}


#' @rdname azure_login
#' @export
get_azure_login <- function(tenant="common", selection=NULL, app=NULL, scopes=NULL, auth_type=NULL, refresh=TRUE)
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

    message("Loading Azure Resource Manager login for ", format_tenant(tenant))

    # do we need to choose which login client to use?
    have_selection <- !is.null(selection)
    have_auth_spec <- any(!is.null(app), !is.null(scopes), !is.null(auth_type))

    token <- if(length(this_login) > 1 || have_selection || have_auth_spec)
        choose_token(this_login, selection, app, scopes, auth_type)
    else load_azure_token(this_login)

    if(is.null(token))
        return(NULL)

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
        sapply(tenant,
            function(hash) az_rm$new(token=load_azure_token(hash)),
            simplify=FALSE)
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


# algorithm for choosing a token:
# if given a hash, choose it (error if no match)
# otherwise if given a number, use it (error if out of bounds)
# otherwise if given any of app|scopes|auth_type, use those (error if no match, ask if multiple matches)
# otherwise ask
choose_token <- function(hashes, selection, app, scopes, auth_type)
{
    if(is.character(selection))
    {
        if(!(selection %in% hashes))
            stop("Token with selected hash not found", call.=FALSE)
        return(load_azure_token(selection))
    }

    if(is.numeric(selection))
    {
        if(selection <= 0 || selection > length(hashes))
            stop("Invalid numeric selection", call.=FALSE)
        return(load_azure_token(hashes[selection]))
    }

    tokens <- lapply(hashes, load_azure_token)
    ok <- rep(TRUE, length(tokens))

    # filter down list of tokens based on auth criteria
    if(!is.null(app) || !is.null(scopes) || !is.null(auth_type))
    {
        if(!is.null(scopes))
            scopes <- tolower(scopes)

        # look for matching token
        for(i in seq_along(hashes))
        {
            app_match <- scope_match <- auth_match <- TRUE

            if(!is.null(app) && tokens[[i]]$client$client_id != app)
                app_match <- FALSE
            if(!is.null(scopes))
            {
                # AAD v1.0 tokens do not have scopes
                if(is.null(tokens[[i]]$scope))
                    scope_match <- is.na(scopes)
                else
                {
                    tok_scopes <- tolower(basename(grep("^.+://", tokens[[i]]$scope, value=TRUE)))
                    if(!setequal(scopes, tok_scopes))
                        scope_match <- FALSE
                }
            }
            if(!is.null(auth_type) && tokens[[i]]$auth_type != auth_type)
                auth_match <- FALSE

            if(!app_match || !scope_match || !auth_match)
                ok[i] <- FALSE
        }
    }

    tokens <- tokens[ok]
    if(length(tokens) == 0)
        stop("No tokens found with selected authentication parameters", call.=FALSE)
    else if(length(tokens) == 1)
        return(tokens[[1]])

    # bring up a menu
    tenant <- tokens[[1]]$tenant
    choices <- sapply(tokens, function(token)
    {
        app <- token$client$client_id
        scopes <- if(!is.null(token$scope))
            paste(tolower(basename(grep("^.+://", token$scope, value=TRUE))), collapse=" ")
        else "<NA>"
        paste0("App ID: ", app,
               "\n   Scopes: ", scopes,
               "\n   Authentication method: ", token$auth_type,
               "\n   MD5 Hash: ", token$hash())
    })
    msg <- paste0("Choose a Microsoft Graph login for ", format_tenant(tenant))
    selection <- utils::menu(choices, title=msg)
    if(selection == 0)
        invisible(NULL)
    else tokens[[selection]]
}

