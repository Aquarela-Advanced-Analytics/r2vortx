#' Get information
#'
#' Gets information on a VORTX job
#'
#' @param key String. User API Key for VORTX.
#' @param job String or List. Can be either a job ID number
#' in string format or parsed JSON in list format,
#' result of organizer or discoverer functions.
#' @param info String. Defines what kind of information to be extracted from job,
#' between 'clusters', 'varscores', 'dataset' or 'summary'. Default is all.
#' @param vortx_server Choose server to run Vortx. Can be one of "production", "sandbox", "local" or desired URL.
#' @return List of DataFrames or DataFrame.
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
vortx_info <- function(key, job, info=c("clusters", "varscores", "dataset", "summary"), vortx_server="production"){

  # Initialize data
  ls <- list()

  # Get necessary data
  if ("clusters" %in% info) {
    hierarchy <- get_hierarchy(key, job, vortx_server)
    ls[["Clusters"]] <- hierarchy
  }

  if ("varscores" %in% info) {
    metrics <- get_metrics(key, job, vortx_server)
    ls[['VarScores']] <- metrics
  }

  if (any(c("dataset", "summary") %in% info)) {
    dataset <- get_dataset(key, job, vortx_server)
    clusters <- levels(dataset[,'clusterId'])

    if ("dataset" %in% info) {
      ls[['Dataset']] <- dataset
    }

    if ("summary" %in% info) {
      summary <- list()
      for (i in clusters){
        name <- i
        cluster_name <- gsub('-', '', name)
        summary[[cluster_name]] <- get_summaryview_single(key, job, name)
      }
      ls[['Summary']] <- summary
    }
  }

  # Return final object
  if (length(ls) == 1) {
    return(ls[[1]])
  } else {
    return(ls)
  }
}
