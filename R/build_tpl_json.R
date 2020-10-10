#' Build the JSON for a template and its parameters
#'
#' @param ... For `build_template_parameters`, named arguments giving the values of each template parameter. For `build_template_definition`, further arguments passed to class methods.
#' @param parameters For `build_template_definition`, the parameter names and types for the template. See 'Details' below.
#' @param variables Internal variables used by the template.
#' @param functions User-defined functions used by the template.
#' @param resources List of resources that the template should deploy.
#' @param outputs The template outputs.
#' @param schema,content_version,api_profile Less common arguments that can be used to customise the template. See the guide to template syntax on Microsoft Docs, linked below.
#'
#' @details
#' `build_template_definition` is used to generate a template from its components. The main arguments are `parameters`, `variables`, `functions`, `resources` and `outputs`. Each of these can be specified in various ways:
#' - As character strings containing unparsed JSON text.
#' - As an R list of (nested) objects, which will be converted to JSON via `jsonlite::toJSON`.
#' - A connection pointing to a JSON file or object.
#' - For the `parameters` argument, this can also be a character vector containing the types of each parameter.
#'
#' `build_template_parameters` is for creating the list of parameters to be passed along with the template. Its arguments should all be named, and contain either the JSON text or an R list giving the parsed JSON.
#'
#' Both of these are generics and can be extended by other packages to handle specific deployment scenarios, eg virtual machines.
#'
#' @return
#' The JSON text for the template definition and its parameters.
#'
#' @seealso
#' [az_template], [jsonlite::toJSON]
#'
#' [Guide to template syntax](https://docs.microsoft.com/en-us/azure/azure-resource-manager/templates/template-syntax)
#' @examples
#' # dummy example
#' # note that 'resources' arg should be a _list_ of resources
#' build_template_definition(resources=list(list(name="resource here")))
#'
#' # specifying parameters as a list
#' build_template_definition(parameters=list(par1=list(type="string")),
#'                           resources=list(list(name="resource here")))
#'
#' # specifying parameters as a vector
#' build_template_definition(parameters=c(par1="string"),
#'                           resources=list(list(name="resource here")))
#'
#' # a user-defined function
#' build_template_definition(
#'     parameters=c(name="string"),
#'     functions=list(
#'         list(
#'             namespace="mynamespace",
#'             members=list(
#'                 prefixedName=list(
#'                     parameters=list(
#'                         list(name="name", type="string")
#'                     ),
#'                     output=list(
#'                         type="string",
#'                         value="[concat('AzureR', parameters('name'))]"
#'                     )
#'                 )
#'             )
#'         )
#'     )
#' )
#'
#' # realistic example: storage account
#' build_template_definition(
#'     parameters=c(
#'         name="string",
#'         location="string",
#'         sku="string"
#'     ),
#'     variables=list(
#'         id="[resourceId('Microsoft.Storage/storageAccounts', parameters('name'))]"
#'     ),
#'     resources=list(
#'         list(
#'             name="[parameters('name')]",
#'             location="[parameters('location')]",
#'             type="Microsoft.Storage/storageAccounts",
#'             apiVersion="2018-07-01",
#'             sku=list(
#'                 name="[parameters('sku')]"
#'             ),
#'             kind="Storage"
#'         )
#'     ),
#'     outputs=list(
#'         storageId="[variables('id')]"
#'     )
#' )
#'
#' # providing JSON text as input
#' build_template_definition(
#'     parameters=c(name="string", location="string", sku="string"),
#'     resources='[
#'         {
#'             "name": "[parameters(\'name\')]",
#'             "location": "[parameters(\'location\')]",
#'             "type": "Microsoft.Storage/storageAccounts",
#'             "apiVersion": "2018-07-01",
#'             "sku": {
#'                 "name": "[parameters(\'sku\')]"
#'             },
#'             "kind": "Storage"
#'         }
#'     ]'
#' )
#'
#' # parameter values
#' build_template_parameters(name="mystorageacct", location="westus", sku="Standard_LRS")
#'
#' build_template_parameters(
#'     param='{
#'         "name": "myname",
#'         "properties": { "prop1": 42, "prop2": "hello" }
#'     }'
#' )
#'
#' param_json <- '{
#'         "name": "myname",
#'         "properties": { "prop1": 42, "prop2": "hello" }
#'     }'
#' build_template_parameters(param=textConnection(param_json))
#'
#' \dontrun{
#' # reading JSON definitions from files
#' build_template_definition(
#'     parameters=file("parameter_def.json"),
#'     resources=file("resource_def.json")
#' )
#'
#' build_template_parameters(name="myres_name", complex_type=file("myres_params.json"))
#' }
#'
#' @rdname build_template
#' @aliases build_template
#' @export
build_template_definition <- function(...)
{
    UseMethod("build_template_definition")
}


#' @rdname build_template
#' @export
build_template_definition.default <- function(
    parameters=named_list(), variables=named_list(), functions=list(), resources=list(), outputs=named_list(),
    schema="https://schema.management.azure.com/schemas/2019-04-01/deploymentTemplate.json#",
    content_version="1.0.0.0",
    api_profile=NULL,
    ...)
{
    # special treatment for parameters arg: convert 'c(name="type")' to 'list(name=list(type="type"))'
    if(is.character(parameters))
        parameters <- sapply(parameters, function(type) list(type=type), simplify=FALSE)

    parts <- lapply(
        list(
            `$schema`=schema,
            contentVersion=content_version,
            apiProfile=api_profile,
            parameters=parameters,
            variables=variables,
            functions=functions,
            resources=resources,
            outputs=outputs,
            ...
        ),
        function(x)
        {
            if(inherits(x, "connection"))
            {
                on.exit(close(x))
                readLines(x)
            }
            else generate_json(x)
        }
    )
    parts <- parts[parts != "null"]
    # json <- "{}"
    # for(i in seq_along(parts))
    #     if(parts[i] != "null")
    #         json <- do.call(append_json, c(json, parts[i]))

    jsonlite::prettify(do.call(append_json, c(list("{}"), parts)))
}


#' @rdname build_template
#' @export
build_template_parameters <- function(...)
{
    UseMethod("build_template_parameters")
}


#' @rdname build_template
#' @export
build_template_parameters.default <- function(...)
{
    dots <- list(...)

    # handle no-parameter case
    if(is_empty(dots))
        return("{}")

    parms <- lapply(dots, function(value)
    {
        # need to duplicate functionality of generate_json, one level down
        if(inherits(value, "connection"))
        {
            on.exit(close(value))
            generate_json(list(value=jsonlite::fromJSON(readLines(value), simplifyVector=FALSE)))
        }
        else if(is.character(value) && jsonlite::validate(value))
            generate_json(list(value=jsonlite::fromJSON(value, simplifyVector=FALSE)))
        else generate_json(list(value=value))
    })

    jsonlite::prettify(do.call(append_json, c(list("{}"), parms)))
}


generate_json <- function(object)
{
    if(is.character(object) && jsonlite::validate(object))
        object
    else jsonlite::toJSON(object, auto_unbox=TRUE, null="null", digits=22)
}


append_json <- function(props, ...)
{
    lst <- list(...)
    lst_names <- names(lst)
    if(is.null(lst_names) || any(lst_names == ""))
        stop("Deployment properties and parameters must be named", call.=FALSE)

    for(i in seq_along(lst))
    {
        lst_i <- lst[[i]]
        if(inherits(lst_i, "connection"))
        {
            on.exit(close(lst_i))
            lst_i <- readLines(lst_i)
        }

        newprop <- sprintf('"%s": %s}', lst_names[i], paste0(lst_i, collapse="\n"))
        if(!grepl("^\\{[[:space:]]*\\}$", props))
            newprop <- paste(",", newprop)
        props <- sub("\\}$", newprop, props)
    }

    props
}


is_file_spec <- function(x)
{
    inherits(x, "connection") || (is.character(x) && length(x) == 1 && file.exists(x))
}
