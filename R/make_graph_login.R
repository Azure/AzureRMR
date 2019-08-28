make_graph_login_from_token <- function(tenant, token)
{
    if(!requireNamespace("AzureGraph"))
        return()

    message("Also creating Microsoft Graph login for ", format_tenant(token$tenant))
    newtoken <- token$clone()
    if(is_azure_v1_token(newtoken))
        newtoken$resource <- "https://graph.microsoft.com/"
    else newtoken$scope <- sub("management\\.azure\\.com", "graph\\.microsoft\\.com", newtoken$scope)

    newtoken$refresh()

    AzureGraph::create_graph_login(tenant=normalize_tenant(tenant), token=newtoken)
}
