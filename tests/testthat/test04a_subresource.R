context("Sub-resources")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_APP_ID")
password <- Sys.getenv("AZ_TEST_PASSWORD")
subscription <- Sys.getenv("AZ_TEST_SUBSCRIPTION")

if(tenant == "" || app == "" || password == "" || subscription == "")
    skip("Resource group method tests skipped: ARM credentials not set")

rgname <- paste(sample(letters, 20, replace=TRUE), collapse="")
rg <- az_rm$
    new(tenant=tenant, app=app, password=password)$
    get_subscription(subscription)$
    create_resource_group(rgname, location="australiaeast")


test_that("Resource methods work",
{
    resname <- paste0(sample(letters, 20), collapse="")
    # resource with sub-resources
    res <- rg$create_resource(type="Microsoft.Storage/storageAccounts", name=resname,
        kind="StorageV2",
        sku=list(name="Standard_LRS"),
        wait=TRUE)
    expect_true(is_resource(res))

    subresname <- paste0(sample(letters, 20), collapse="")
    subres <- res$create_subresource(type="blobservices/default/containers", name=subresname)
    expect_true(is_resource(subres))

    expect_true(is_resource(res$get_subresource(type="blobservices/default/containers", name=subresname)))

    expect_message(res$delete_subresource(type="blobservices/default/containers", name=subresname, confirm=FALSE))
})

rg$delete(confirm=FALSE)


