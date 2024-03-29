% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_tpl_json.R
\name{build_template_definition}
\alias{build_template_definition}
\alias{build_template}
\alias{build_template_definition.default}
\alias{build_template_parameters}
\alias{build_template_parameters.default}
\title{Build the JSON for a template and its parameters}
\usage{
build_template_definition(...)

\method{build_template_definition}{default}(parameters = named_list(),
  variables = named_list(), functions = list(), resources = list(),
  outputs = named_list(), schema = "2019-04-01", version = "1.0.0.0",
  api_profile = NULL, ...)

build_template_parameters(...)

\method{build_template_parameters}{default}(...)
}
\arguments{
\item{...}{For \code{build_template_parameters}, named arguments giving the values of each template parameter. For \code{build_template_definition}, further arguments passed to class methods.}

\item{parameters}{For \code{build_template_definition}, the parameter names and types for the template. See 'Details' below.}

\item{variables}{Internal variables used by the template.}

\item{functions}{User-defined functions used by the template.}

\item{resources}{List of resources that the template should deploy.}

\item{outputs}{The template outputs.}

\item{schema, version, api_profile}{Less commonly used arguments that can be used to customise the template. See the guide to template syntax on Microsoft Docs, linked below.}
}
\value{
The JSON text for the template definition and its parameters.
}
\description{
Build the JSON for a template and its parameters
}
\details{
\code{build_template_definition} is used to generate a template from its components. The main arguments are \code{parameters}, \code{variables}, \code{functions}, \code{resources} and \code{outputs}. Each of these can be specified in various ways:
\itemize{
\item As character strings containing unparsed JSON text.
\item As an R list of (nested) objects, which will be converted to JSON via \code{jsonlite::toJSON}.
\item A connection pointing to a JSON file or object.
\item For the \code{parameters} argument, this can also be a character vector containing the types of each parameter.
}

\code{build_template_parameters} is for creating the list of parameters to be passed along with the template. Its arguments should all be named, and contain either the JSON text or an R list giving the parsed JSON.

Both of these are generics and can be extended by other packages to handle specific deployment scenarios, eg virtual machines.
}
\examples{
# dummy example
# note that 'resources' arg should be a _list_ of resources
build_template_definition(resources=list(list(name="resource here")))

# specifying parameters as a list
build_template_definition(parameters=list(par1=list(type="string")),
                          resources=list(list(name="resource here")))

# specifying parameters as a vector
build_template_definition(parameters=c(par1="string"),
                          resources=list(list(name="resource here")))

# a user-defined function
build_template_definition(
    parameters=c(name="string"),
    functions=list(
        list(
            namespace="mynamespace",
            members=list(
                prefixedName=list(
                    parameters=list(
                        list(name="name", type="string")
                    ),
                    output=list(
                        type="string",
                        value="[concat('AzureR', parameters('name'))]"
                    )
                )
            )
        )
    )
)

# realistic example: storage account
build_template_definition(
    parameters=c(
        name="string",
        location="string",
        sku="string"
    ),
    variables=list(
        id="[resourceId('Microsoft.Storage/storageAccounts', parameters('name'))]"
    ),
    resources=list(
        list(
            name="[parameters('name')]",
            location="[parameters('location')]",
            type="Microsoft.Storage/storageAccounts",
            apiVersion="2018-07-01",
            sku=list(
                name="[parameters('sku')]"
            ),
            kind="Storage"
        )
    ),
    outputs=list(
        storageId="[variables('id')]"
    )
)

# providing JSON text as input
build_template_definition(
    parameters=c(name="string", location="string", sku="string"),
    resources='[
        {
            "name": "[parameters(\'name\')]",
            "location": "[parameters(\'location\')]",
            "type": "Microsoft.Storage/storageAccounts",
            "apiVersion": "2018-07-01",
            "sku": {
                "name": "[parameters(\'sku\')]"
            },
            "kind": "Storage"
        }
    ]'
)

# parameter values
build_template_parameters(name="mystorageacct", location="westus", sku="Standard_LRS")

build_template_parameters(
    param='{
        "name": "myname",
        "properties": { "prop1": 42, "prop2": "hello" }
    }'
)

param_json <- '{
        "name": "myname",
        "properties": { "prop1": 42, "prop2": "hello" }
    }'
build_template_parameters(param=textConnection(param_json))

\dontrun{
# reading JSON definitions from files
build_template_definition(
    parameters=file("parameter_def.json"),
    resources=file("resource_def.json")
)

build_template_parameters(name="myres_name", complex_type=file("myres_params.json"))
}

}
\seealso{
\link{az_template}, \link[jsonlite:fromJSON]{jsonlite::toJSON}

\href{https://learn.microsoft.com/en-us/azure/azure-resource-manager/templates/template-syntax}{Guide to template syntax}
}
