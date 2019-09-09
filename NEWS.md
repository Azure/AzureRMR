# AzureRMR 2.1.3.9000

- If the AzureGraph package is installed and the `graph_host` argument is not `NULL`, `create_azure_login` will also create a login client for Microsoft Graph with the same credentials. This is to facilitate working with registered apps and service principals, eg when managing roles and permissions. Some Azure services also require creating service principals as part of creating a resource (eg Azure Kubernetes Service), and keeping the Graph credentials consistent with ARM helps ensure nothing breaks.
- Fix a bug where `create_azure_login` still required the `tenant` argument when a token was supplied.
- Fixes to allow use of Azure Active Directory v2.0 tokens for authenticating. Note that AAD v1.0 is still the default and recommended version.

# AzureRMR 2.1.3

- Fix a bug where failure to create a resource would not be detected.
- Make setting tags more robust (some resources return a null tags field when no tags are present, rather than an empty object).
- Better handling of null fields for all REST calls.

# AzureRMR 2.1.2

- Fix a bug in template deployment where null fields were not handled correctly.
- New `build_template_definition` and `build_parameters_parameters` generics to help in template deployment. These can take as inputs R lists, JSON text strings, or file connections, and can also be extended by other packages.

# AzureRMR 2.1.1

* Some refactoring of login code to better handle AzureAuth options. As part of this, the `config_file` argument for `az_rm$new` has been removed; to use a configuration file, call the (recommended) `create_azure_login` function.
* `az_subscription$get_provider_api_version` now returns only stable APIs by default. Set the argument `stable_only=FALSE` to allow returning preview APIs.

# AzureRMR 2.1.0

* This version adds basic support for role-based access control (RBAC) at subscription, resource group and resource level. Add and remove role assignments, and retrieve role definitions. See `?rbac` for more information.
* Fix a bug where if the user decides not to create a caching dir when prompted by AzureAuth, AzureRMR would pop up a second prompt.
* Internal refactoring to remove duplicated code.

# AzureRMR 2.0.0

## Significant interface changes

* New `create_azure_login`, `get_azure_login` and `delete_azure_login` functions to handle ARM authentication. By default, these will authenticate using your AAD user credentials without requiring you to create a service principal. Directly calling `az_rm$new()` will still work, but it's recommended to use `create_azure_login` and `get_azure_login` going forward. Login credentials will be saved and reused for subsequent sessions (see below).
* Token acquisition logic substantially enhanced and moved to a new package, [AzureAuth](https://github.com/cloudyr/AzureAuth). `get_azure_token` now supports four authentication methods for obtaining tokens (`client_credentials`, `authorization_code`, `device_code` and `resource_owner`). Tokens are also automatically cached and retrieved for use in subsequent sessions, without needing the user to reauthenticate. See the AzureAuth documentation for more details.

## Other changes

* Don't print empty fields for ARM objects.
* Add optional `etag` field to resource object definition.
* Add `location` argument to `az_resource_group$create_resource` method, rather than hardcoding it to the resgroup location.
* Add `wait` argument when creating a new resource, similar to deploying a template, since some resources will return before provisioning is complete. Defaults to `FALSE` for backward compatibility.
* Export `is_azure_token`.
* Allow `az_resource_group$deploy_template()` to work without `parameters` arg (parameters folded into template itself).
* Fix a bug that kept `az_resource_group$delete_resource` from deleting the resource.
* New resource method `az_resource$get_api_version` to match `set_api_version`.
* By default, use the latest _stable_ API version when interacting with resources. `az_resource$set_api_version` gains a new argument `stable_only` which defaults to `TRUE`; set this to `FALSE` if you want the latest preview version.
* `az_resource$sync_fields()` will respect a non-default API version.
* Add management lock functionality for subscriptions, resource groups and resources. Call `create_lock` to create a lock, `get_lock` to retrieve an existing lock object, and `delete_lock` to delete a lock. Call `list_locks` to list all the locks that apply to an object.
* Add tagging functionality for resource groups, similar to that for resources. Call `set_tags` to set tags, and `get_tags` to retrieve them.
* Allow `named_list` to accept empty inputs. The output will be a list of length 0 with a `names` attribute.
* Fix template deployment not to drop empty fields (related to #13).

# AzureRMR 1.0.0

* Submitted to CRAN

# AzureRMR 0.9.0

* Moved to cloudyr organisation
