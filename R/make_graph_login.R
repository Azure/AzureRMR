make_graph_login_from_token <- function(token, azure_host, graph_host)
{
    if(is_empty(graph_host))
        return()

    message("Also creating Microsoft Graph login for ", format_tenant(token$tenant))
    newtoken <- token$clone()
    if(is_azure_v1_token(newtoken))
        newtoken$resource <- graph_host
    else newtoken$scope <- c(paste0(graph_host, ".default"), "openid", "offline_access")

    newtoken$refresh()

    res <- try(AzureGraph::create_graph_login(tenant=token$tenant, token=newtoken))
    if(inherits(res, "try-error"))
        warning("Unable to create Microsoft Graph login", call.=FALSE)
}
