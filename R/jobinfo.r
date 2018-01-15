#' Get information
#'
#' Gets information on a VORTX job
#'
#' @param key String. User API Key for VORTX.
#' @param job String or List. Can be either a job ID number
#' in string format or parsed JSON in list format,
#' result of organizer or discoverer functions.
#' @param info String. Defines what kind of information to be extracted from job,
#' between 'clusters', 'varscores', 'dataset' or 'summary'. Default is 'all'.
#' @param sandbox Choose TRUE if job is to be sent to sandbox instead
#' @return List of DataFrames or DataFrame. If 'all' or 'summary' are defined, a list. Else specific DataFrame.
#' @export
#' @examples
#' \dontrun{
#' mykey <- '1234567890abcefghijkl'
#' myjobid <- '0987654321'
#'
#' myjobname <- 'My job'
#' myjobdesc <- 'This is a job that does job stuff'
#' df <- data.frame(c(1,2,3), c(4,5,6), c(7,8,9))
#' myjob <- vortx_create_job(mykey, df, myjobname, myjobdesc)
#'
#' vortx_info(mykey, myjobid, info = 'clusters')
#' vortx_info(mykey, myjob)
#' }
vortx_info <- function(key, job, info='all', sandbox=FALSE){

    # Temporary data
    hierarchy <- get_hierarchy(key, job, sandbox)
    metrics <- get_metrics(key, job, sandbox)
    dataset <- get_dataset(key, job, sandbox)
    clusters <- levels(dataset[,'clusterId'])

    # Conditionals
    if (info == 'clusters'){
        return(hierarchy)
    }

    if (info == 'varscores'){
        return(metrics)
    }

    if (info == 'dataset'){
        return(dataset)
    }

    # Summary for last to save requests if unnecessary
    summary <- list()
    for (i in clusters){
        name <- i
        cluster_name <- gsub('-', '', name)
        summary[[cluster_name]] <- get_summaryview_single(key, job, name)
    }
    if (info == 'summary'){
        return(summary)
    }

    # Function response
    ls <- list()
    ls[['Clusters']] <- hierarchy
    ls[['VarScores']] <- metrics
    ls[['Dataset']] <- dataset
    ls[['Summary']] <- summary

    return(ls)

}
