context("Subscriptions")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_APP_ID")
password <- Sys.getenv("AZ_TEST_PASSWORD")
subscription <- Sys.getenv("AZ_TEST_SUBSCRIPTION")

if(tenant == "" || app == "" || password == "" || subscription == "")
    skip("Subscription method tests skipped: ARM credentials not set")


test_that("Subscription methods work",
{
    az <- az_rm$new(tenant=tenant, app=app, password=password)

    subs <- az$list_subscriptions()
    expect_true(is.list(subs) && all(sapply(subs, is_subscription)))

    sub <- az$get_subscription(subscription)
    expect_is(sub, "az_subscription")

    locs <- sub$list_locations()
    expect_is(locs, "data.frame")

    vers <- sub$get_provider_api_version()
    expect_true(is.list(vers))

    vers_comp <- sub$get_provider_api_version("Microsoft.Compute")
    expect_true(is.character(vers_comp))

    vers_vm <- sub$get_provider_api_version("Microsoft.Compute", "virtualMachines")
    expect_true(is.character(vers_vm) && length(vers_vm) == 1)

    # assume there are actually resource groups here
    rgs <- sub$list_resource_groups()
    expect_true(is.list(rgs) && all(sapply(rgs, is_resource_group)))

    # assume there are actually resources
    res <- sub$list_resources()
    expect_true(is.list(res) && all(sapply(res, is_resource)))

    # locks (minimal testing at sub level right now)
    locks <- sub$list_locks()
    expect_true(is.list(locks))

    res <- sub$do_operation("resourceGroups")
    expect_true(is.list(res))
})

test_that("List filters work",
{
    az <- az_rm$new(tenant=tenant, app=app, password=password)
    sub <- az$get_subscription(subscription)

    # assume there are 2 or more resource groups
    rgs <- sub$list_resource_groups(top=1)
    expect_true(is.list(rgs) && all(sapply(rgs, is_resource_group)) && length(rgs) == 1)

    # assume there is a resource group with this tag
    rgs <- sub$list_resource_groups(filter="tagName eq 'createdBy' and tagValue eq 'AzureR/AzureRMR'")
    expect_true(is.list(rgs) && all(sapply(rgs, is_resource_group)))

    # assume there is a resource of this type
    res <- sub$list_resources(filter="resourceType eq 'Microsoft.Storage/storageAccounts'", expand="createdTime")
    expect_true(is.list(res))
    expect_true(all(sapply(res,
        function(r) is_resource(r) && r$type == "Microsoft.Storage/storageAccounts" && !is_empty(r$ext$createdTime))))
})

test_that("Subscription methods work with AAD v1.0",
{
    token <- get_azure_token("https://management.azure.com/",
                            tenant=tenant, app=app, password=password, version=1)
    az <- az_rm$new(token=token)

    subs <- az$list_subscriptions()
    expect_true(is.list(subs) && all(sapply(subs, is_subscription)))

    sub <- az$get_subscription(subscription)
    expect_is(sub, "az_subscription")
})
