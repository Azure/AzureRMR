# AzureRMR

[![CRAN](https://www.r-pkg.org/badges/version/AzureRMR)](https://cran.r-project.org/package=AzureRMR)
![Downloads](https://cranlogs.r-pkg.org/badges/AzureRMR)
[![Travis Build Status](https://travis-ci.org/cloudyr/AzureRMR.png?branch=master)](https://travis-ci.org/cloudyr/AzureRMR)

AzureRMR is a package for interacting with Azure Resource Manager: authenticate, list subscriptions, manage resource groups, deploy and delete templates and resources. It calls the Resource Manager [REST API](https://docs.microsoft.com/en-us/rest/api/resources) directly, so you don't need to have PowerShell or Python installed.

You can install the development version from GitHub, via `devtools::install_github("cloudyr/AzureRMR")`.


## Before you begin

To use AzureRMR, you must create and register a service principal with Azure Active Directory. This is a one-time task, and the easiest method is to use the Azure cloud shell.

- In the Azure Portal (https://portal.azure.com/), click on the Cloud Shell icon:

![](vignettes/images/cloudportal2.png)

- If you haven't used the shell before, there will be a dialog box to choose whether to use bash or PowerShell. Choose bash.
- In the shell, type `az ad sp create-for-rbac --name {app-name} --subscription "{your-subscription-name}" --years {N}`, substituting the desired name of your service principal (try to make it memorable to you, and unlikely to clash with other names), your subscription name, and the number of years you want the password to be valid.
- Wait until the app creation is complete. You should see a screen like this.

![](vignettes/images/cloudshell.png)

- Record your tenant ID, app ID, and password.

If you want to allow access at something other than subscription level, you can use the `--scopes` argument in place of `--subscription`. For example, to restrict AzureRMR to only the "AnalyticsRG" resource group: `az ad sp create-for-rbac --scopes /subscriptions/{your-subscription-ID}/resourceGroups/AnalyticsRG`.


## Authentication

Under the hood, AzureRMR uses a similar authentication process to the [Azure CLI](https://docs.microsoft.com/en-us/cli/azure/?view=azure-cli-latest). The first time you authenticate with a given Azure Active Directory tenant, you call `create_azure_login()` and supply your tenant, app ID and password. The resulting Resource Manager client object is saved on your machine, and can be retrieved in subsequent R sessions with `get_azure_login("{tenant}")`. AzureRMR will automatically refresh your credentials so you don't have to re-authenticate.


## Sample workflow

```r
library(AzureRMR)

# authenticate with Azure AD:
# - on first login to this client, call create_azure_login(...)
# - on subsequent logins, call get_azure_login("myaadtenant")
az <- create_azure_login("myaadtenant.onmicrosoft.com", app="app_id", password="password")

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

# delete them
stor2$delete(confirm=FALSE)
rg2$delete(confirm=FALSE)
```

## Extending

AzureRMR is meant to be a generic mechanism for working with Resource Manager. You can extend it to provide support for service-specific features; examples of packages that do this include [AzureVM](https://github.com/cloudyr/AzureVM) for [virtual machines](https://azure.microsoft.com/en-us/services/virtual-machines/), and [AzureStor](https://github.com/cloudyr/AzureStor) for [storage accounts](https://azure.microsoft.com/en-us/services/storage/). For more information, see the ["Extending AzureRMR" vignette](vignettes/extend.Rmd).

---
[![cloudyr project logo](https://i.imgur.com/JHS98Y7.png)](https://github.com/cloudyr)
