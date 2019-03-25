# AzureRMR

[![CRAN](https://www.r-pkg.org/badges/version/AzureRMR)](https://cran.r-project.org/package=AzureRMR)
![Downloads](https://cranlogs.r-pkg.org/badges/AzureRMR)
[![Travis Build Status](https://travis-ci.org/cloudyr/AzureRMR.png?branch=master)](https://travis-ci.org/cloudyr/AzureRMR)

AzureRMR is a package for interacting with Azure Resource Manager: list subscriptions, manage resource groups, deploy and delete templates and resources. It calls the Resource Manager [REST API](https://docs.microsoft.com/en-us/rest/api/resources) directly, so you don't need to have PowerShell or Python installed. Azure Active Directory OAuth tokens are obtained using the [AzureAuth](https://github.com/cloudyr/AzureAuth) package.

You can install the development version from GitHub, via `devtools::install_github("cloudyr/AzureRMR")`.


## Authentication

Under the hood, AzureRMR uses a similar authentication process to the [Azure CLI](https://docs.microsoft.com/en-us/cli/azure/?view=azure-cli-latest). The first time you authenticate with a given Azure Active Directory tenant, you call `create_azure_login()` and supply your credentials. AzureRMR will prompt you for permission to create a special data directory in which to cache the obtained authentication token and Resource Manager login. Once this information is saved on your machine, it can be retrieved in subsequent R sessions with `get_azure_login()`. Your credentials will be automatically refreshed so you don't have to reauthenticate.

Unless you have a specific reason otherwise, it's recommended that you allow AzureRMR to create this caching directory. Note that many other cloud engineering tools save credentials in this way, including the Azure CLI itself.

In most cases, AzureRMR can authenticate without requiring you to create your own service principal. However, AzureRMR can also use a custom service principal, and in general it's a good idea to supply your own to authenticate with (if possible). See the ["Introduction to AzureRMR"](vignettes/intro.Rmd) vignette for more details.

**Linux DSVM note** If you are using a Linux [Data Science Virtual Machine](https://azure.microsoft.com/en-us/services/virtual-machines/data-science-virtual-machines/) in Azure, you may have problems running `create_azure_login()`. In this case, try `create_azure_login(auth_type="device_code")`.


## Sample workflow

```r
library(AzureRMR)

# authenticate with Azure AD:
# - on first login to this client, call create_azure_login()
# - on subsequent logins, call get_azure_login()
az <- create_azure_login()

# get a subscription and resource group
sub <- az$get_subscription("{subscription_id}")
rg <- sub$get_resource_group("rgname")

# get a resource (storage account)
stor <- rg$get_resource(type="Microsoft.Storage/storageAccounts", name="mystorage")

# method chaining works too
stor <- az$
    get_subscription("{subscription_id}")$
    get_resource_group("rgname")$
    get_resource(type="Microsoft.Storage/storageAccounts", name="mystorage")


# create a new resource group and resource
rg2 <- sub$create_resource_group("newrgname", location="westus")

stor2 <- rg2$create_resource(type="Microsoft.Storage/storageAccounts", name="mystorage2",
    kind="Storage", sku=list(name="Standard_LRS"))

# tagging
stor2$set_tags(comment="hello world!", created_by="AzureRMR")

# role-based access control (RBAC)
# this uses the AzureGraph package to retrieve the user ID
gr <- AzureGraph::get_graph_login()
usr <- gr$get_user("username@aadtenant.com")
stor2$add_role_assignment(usr, "Storage blob data contributor")
```

## Extending

AzureRMR is meant to be a generic mechanism for working with Resource Manager. You can extend it to provide support for service-specific features; examples of packages that do this include [AzureVM](https://github.com/cloudyr/AzureVM) for [virtual machines](https://azure.microsoft.com/en-us/services/virtual-machines/), and [AzureStor](https://github.com/cloudyr/AzureStor) for [storage accounts](https://azure.microsoft.com/en-us/services/storage/). For more information, see the ["Extending AzureRMR" vignette](vignettes/extend.Rmd).

## Acknowledgements

AzureRMR is inspired by the package AzureSMR, originally written by Alan Weaver and Andrie de Vries, and would not have been possible without their pioneering work. Thanks, guys!

---
[![cloudyr project logo](https://i.imgur.com/JHS98Y7.png)](https://github.com/cloudyr)
