context("Template builders")


test_that("Template definition builder works",
{
    expect_silent(tpl0 <- build_template_definition())
    # make sure test doesn't fail on extraneous newlines at end
    expect_identical(
        sub("\n+$", "", unclass(tpl0)),
        sub("\n+$", "", paste0(readLines("../resources/template_null.json"), collapse="\n"))
    )

    expect_silent(tpl1 <- build_template_definition(parameters=c(parm1="string", parm2="string")))

    tpl2 <- build_template_definition(
        parameters=list(parm1=list(type="string"), parm2=list(type="string")))
    expect_identical(tpl1, tpl2)

    expect_silent(tpl3 <- build_template_definition(
        resource=list(
            list(
                name="resname", type="resprovider/type", properties=list(prop1=42, prop2="hello")
            )
        )
    ))

    res_str <- '[
        {
            "name":"resname", "type":"resprovider/type", "properties":{ "prop1": 42, "prop2": "hello" }
        }
    ]'
    tpl4 <- build_template_definition(resource=res_str)
    expect_identical(tpl3, tpl4)

    tpl5 <- build_template_definition(resource=textConnection(res_str))
    expect_identical(tpl3, tpl5)

    expect_silent(tpl6 <- build_template_definition(
        parameters=file("../resources/parameters.json"),
        functions=file("../resources/functions.json"),
        resources=file("../resources/resources.json")
    ))
    expect_identical(
        sub("\n+$", "", unclass(tpl6)),
        sub("\n+$", "", paste0(readLines("../resources/template.json"), collapse="\n"))
    )
})


test_that("Template parameters builder works",
{
    expect_identical(build_template_parameters(), "{}")

    expect_silent(build_template_parameters(parm1="foo", parm2=list(bar="hello")))

    expect_silent(par1 <- build_template_parameters(parm=file("../resources/parameter_values.json")))

    par2 <- build_template_parameters(parm=readLines("../resources/parameter_values.json"))
    expect_identical(par1, par2)

    parm_str <- paste0(readLines("../resources/parameter_values.json"), collapse="\n")
    par3 <- build_template_parameters(parm=textConnection(parm_str))
    expect_identical(par1, par3)

    par4 <- build_template_parameters(
        parm=jsonlite::fromJSON("../resources/parameter_values.json", simplifyVector=FALSE))
    expect_identical(par1, par4)
})
