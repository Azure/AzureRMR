context("Templates")

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


test_that("Template methods work",
{
    # precondition
    expect_true(is_empty(rg$list_resources()))

    # simple storage account template
    tplname <- paste(sample(letters, 10, replace=TRUE), collapse="")
    template <- "../resources/template.json"
    parameters <- jsonlite::toJSON(list(
        location=list(value="australiaeast"),
        name=list(value=tplname)
    ), auto_unbox=TRUE)

    tpl <- rg$deploy_template(tplname, template=template, parameters=parameters, wait=TRUE)
    tpl$check()
    expect_is(tpl, "az_template")
    expect_false(is_empty(rg$list_resources()))

    tpl$delete(confirm=FALSE, free_resources=TRUE)
    expect_true(is_empty(rg$list_resources()))

    tplname2 <- paste(sample(letters, 10, replace=TRUE), collapse="")
    tpl_parsed <- jsonlite::fromJSON(template, simplifyVector=FALSE)
    parm_parsed <- list(
        location="australiaeast",
        name=tplname2
    )

    tpl2 <- rg$deploy_template(tplname2, template=tpl_parsed, parameters=parm_parsed, wait=TRUE)
    tpl2$check()
    expect_is(tpl2, "az_template")
    expect_false(is_empty(rg$list_resources()))

    tpl2$delete(confirm=FALSE, free_resources=TRUE)
    Sys.sleep(2)
    expect_true(is_empty(rg$list_resources()))

    # leave out parameters arg, modify template to incorporate parameters
    tplname3 <- paste(sample(letters, 10, replace=TRUE), collapse="")
    tpl_parsed$parameters$location$defaultValue <- "australiaeast"
    tpl_parsed$parameters$name$defaultValue <- tplname3

    tpl3 <- rg$deploy_template(tplname3, template=tpl_parsed, wait=TRUE)
    tpl3$check()
    expect_is(tpl3, "az_template")
    expect_false(is_empty(rg$list_resources()))

    # from template and parameter builder
    tplname4 <- paste(sample(letters, 10, replace=TRUE), collapse="")
    tpl_def <- build_template_definition(
        parameters=file("../resources/parameters.json"),
        resources=file("../resources/resources.json")
    )
    par_def <- build_template_parameters(location="australiaeast", name=tplname4)
    tpl4 <- rg$deploy_template(tplname4, template=tpl_def,  parameters=par_def, wait=TRUE)
    tpl4$check()
    expect_is(tpl4, "az_template")

    # tagging
    expect_identical(tpl4$get_tags(), list(createdBy="AzureR/AzureRMR"))

    # listing
    tpllst0 <- rg$list_templates()
    expect_true(is.list(tpllst0) && all(sapply(tpllst0, is_template)))

    tpllst <- rg$list_templates(top=1)
    expect_true(is.list(tpllst) && length(tpllst) == 1)

    tpllst <- rg$list_templates(filter="provisioningState eq 'Succeeded'")
    expect_true(is.list(tpllst) && length(tpllst) > 0)
})

test_that("Bad templates fail gracefully",
{
    tplname <- paste(sample(letters, 10, replace=TRUE), collapse="")
    template <- "../resources/template_bad.json"
    expect_error(rg$deploy_template(tplname, template=template, wait=TRUE), "Unable to deploy template")
})

rg$delete(confirm=FALSE)
