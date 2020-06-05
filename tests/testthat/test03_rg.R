context("Resource groups")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_APP_ID")
password <- Sys.getenv("AZ_TEST_PASSWORD")
subscription <- Sys.getenv("AZ_TEST_SUBSCRIPTION")

if(tenant == "" || app == "" || password == "" || subscription == "")
    skip("Resource group method tests skipped: ARM credentials not set")

sub <- az_rm$new(tenant=tenant, app=app, password=password)$get_subscription(subscription)


test_that("Resource group methods work",
{
    rgname <- paste(sample(letters, 20, replace=TRUE), collapse="")

    expect_false(sub$resource_group_exists(rgname))

    rgnew <- sub$create_resource_group(rgname, location="westus")
    expect_is(rgnew, "az_resource_group")
    expect_equal(rgnew$name, rgname)
    expect_true(sub$resource_group_exists(rgname))

    rgnew2 <- sub$get_resource_group(rgname)
    expect_is(rgnew2, "az_resource_group")
    expect_equal(rgnew2$name, rgname)

    res <- rgnew$do_operation("resources")
    expect_true(is.list(res))

    # tagging
    rgnew$set_tags(tag1="value1")
    expect_identical(rgnew$get_tags(), list(createdBy="AzureR/AzureRMR", tag1="value1"))
    rgnew$set_tags(tag2)
    expect_identical(rgnew$get_tags(), list(createdBy="AzureR/AzureRMR", tag1="value1", tag2=""))
    rgnew$set_tags(tag2=NULL)
    expect_identical(rgnew$get_tags(), list(createdBy="AzureR/AzureRMR", tag1="value1"))
    rgnew$set_tags(keep_existing=FALSE)
    expect_true(is_empty(rgnew$get_tags()))

    # locking
    expect_is(rgnew$create_lock("newlock_rg", level="cannotdelete"), "az_resource")
    expect_is(rgnew$get_lock("newlock_rg"), "az_resource")
    expect_true({
        locks <- rgnew$list_locks()
        is.list(locks) && all(sapply(locks, is_resource))
    })
    expect_null(rgnew$delete_lock("newlock_rg"))
    expect_error(rgnew$get_lock("newlock_rg"))

    rgnew$delete(confirm=FALSE)
})
