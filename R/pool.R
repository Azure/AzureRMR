#' Manage parallel Azure connections
#'
#' @param size For `init_pool`, the number of background R processes to create. Limit this is you are low on memory.
#' @param restart For `init_pool`, whether to terminate an already running pool first.
#' @param ... Other arguments passed on to functions in the parallel package. See below.
#'
#' @details
#' AzureRMR provides the ability to parallelise communicating with Azure by utilizing a pool of R processes in the background. This often leads to major speedups in scenarios like downloading large numbers of small files, or working with a cluster of virtual machines. This functionality is intended for use by packages that extend AzureRMR (and was originally implemented as part of the AzureStor package), but can also be called directly by the end-user.
#'
#' A small API consisting of the following functions is currently provided for managing the pool. They pass their arguments down to the corresponding functions in the parallel package.
#' - `init_pool` initialises the pool, creating it if necessary. The pool is created by calling `parallel::makeCluster` with the pool size and any additional arguments. If `init_pool` is called and the current pool is smaller than `size`, it is resized.
#' - `delete_pool` shuts down the background processes and deletes the pool.
#' - `pool_exists` checks for the existence of the pool, returning a TRUE/FALSE value.
#' - `pool_size` returns the size of the pool, or zero if the pool does not exist.
#' - `pool_export` exports variables to the pool nodes. It calls `parallel::clusterExport` with the given arguments.
#' - `pool_lapply`, `pool_sapply` and `pool_map` carry out work on the pool. They call `parallel::parLapply`, `parallel::parSapply` and `parallel::clusterMap` with the given arguments.
#' - `pool_call` and `pool_evalq` execute code on the pool nodes. They call `parallel::clusterCall` and `parallel::clusterEvalQ` with the given arguments.
#'
#' The pool is persistent for the session or until terminated by `delete_pool`. You should initialise the pool by calling `init_pool` before running any code on it. This restores the original state of the pool nodes by removing any objects that may be in memory, and resetting the working directory to the master working directory.
#'
#' @seealso
#' [parallel::makeCluster], [parallel::clusterCall], [parallel::parLapply]
#' @examples
#' \dontrun{
#'
#' init_pool()
#'
#' pool_size()
#'
#' x <- 42
#' pool_export("x")
#' pool_sapply(1:5, function(i) i + x)
#'
#' init_pool()
#' # error: x no longer exists on nodes
#' try(pool_sapply(1:5, function(i) i + x))
#'
#' delete_pool()
#'
#' }
#' @rdname pool
#' @export
init_pool <- function(size=10, restart=FALSE, ...)
{
    if(restart || !pool_exists() || pool_size() < size)
    {
        delete_pool()
        message("Creating background pool")
        .AzureR$pool <- parallel::makeCluster(size, ...)
        pool_evalq(loadNamespace("AzureRMR"))
    }
    else
    {
        # restore original state, set working directory to master working directory
        pool_call(function(wd)
        {
            setwd(wd)
            rm(list=ls(envir=.GlobalEnv, all.names=TRUE), envir=.GlobalEnv)
        }, wd=getwd())
    }

    invisible(NULL)
}


#' @rdname pool
#' @export
delete_pool <- function()
{
    if(!pool_exists())
        return(invisible(NULL))

    message("Deleting background pool")
    parallel::stopCluster(.AzureR$pool)
    rm(pool, envir=.AzureR)
}


#' @rdname pool
#' @export
pool_exists <- function()
{
    exists("pool", envir=.AzureR) && inherits(.AzureR$pool, "cluster")
}


#' @rdname pool
#' @export
pool_size <- function()
{
    if(!pool_exists())
        return(0)
    length(.AzureR$pool)
}


#' @rdname pool
#' @export
pool_export <- function(...)
{
    pool_check()
    parallel::clusterExport(cl=.AzureR$pool, ...)
}


#' @rdname pool
#' @export
pool_lapply <- function(...)
{
    pool_check()
    parallel::parLapply(cl=.AzureR$pool, ...)
}


#' @rdname pool
#' @export
pool_sapply <- function(...)
{
    pool_check()
    parallel::parSapply(cl=.AzureR$pool, ...)
}


#' @rdname pool
#' @export
pool_map <- function(...)
{
    pool_check()
    parallel::clusterMap(cl=.AzureR$pool, ...)
}


#' @rdname pool
#' @export
pool_call <- function(...)
{
    pool_check()
    parallel::clusterCall(cl=.AzureR$pool, ...)
}


#' @rdname pool
#' @export
pool_evalq <- function(...)
{
    pool_check()
    parallel::clusterEvalQ(cl=.AzureR$pool, ...)
}


.AzureR <- new.env()


pool_check <- function()
{
    if(!pool_exists())
        stop("AzureR pool does not exist; call init_pool() to create it", call.=FALSE)
}
