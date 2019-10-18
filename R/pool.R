#' Manage parallel Azure connections
#'
#' @param size The number of background R processes to create. Limit this is you are low on memory.
#' @param restart For `init_pool`, whether to terminate an already running pool first.
#' @param ... Other arguments passed on to `parallel::makeCluster`.
#'
#' @details
#' AzureRMR provides the ability to parallelise communicating with Azure by utilizing a pool of R processes in the background. This often leads to major speedups in scenarios like downloading large numbers of small files, or communicating with a cluster of virtual machines. The pool is created by calling `init_pool`. It remains persistent for the session or until terminated by `delete_pool`.
#'
#' If `init_pool` is called and the current pool is smaller than `size`, it is resized.
#'
#' @rdname pool
#' @export
init_pool <- function(size=10, restart=FALSE, ...)
{
    if(restart)
        delete_pool()

    if(!exists("pool", envir=.AzureR) || length(.AzureR$pool) < size)
    {
        delete_pool()
        message("Creating background pool")
        .AzureR$pool <- parallel::makeCluster(size)
        parallel::clusterEvalQ(.AzureR$pool, loadNamespace("AzureStor"))
    }
    else
    {
        # restore original state, set working directory to master working directory
        parallel::clusterCall(.AzureR$pool, function(wd)
        {
            setwd(wd)
            rm(list=ls(all.names=TRUE), envir=.GlobalEnv)
        }, wd=getwd())
    }

    invisible(NULL)
}


#' @rdname pool
#' @export
delete_pool <- function()
{
    if(!exists("pool", envir=.AzureR))
        return()

    message("Deleting background pool")
    parallel::stopCluster(.AzureR$pool)
    rm(pool, envir=.AzureR)
}


#' @export
.AzureR <- new.env()

