make_graph_login_from_token <- function(token, aad_host, graph_host)
{
    if(is_empty(graph_host) || !requireNamespace("AzureGraph", quietly=TRUE))
        return()

    message("Also creating Microsoft Graph login for ", format_tenant(token$tenant))
    newtoken <- token$clone()
    if(is_azure_v1_token(newtoken))
        newtoken$resource <- graph_host
    else newtoken$scope <- sub(aad_host, graph_host, newtoken$scope, fixed=TRUE)

    newtoken$refresh()

    res <- try(AzureGraph::create_graph_login(tenant=token$tenant, token=newtoken))
    if(inherits(res, "try-error"))
        warning("Unable to create Microsoft Graph login", call.=FALSE)
}