#' Get Raw Summaryview
#'
#' Gets raw summaryview of job in VORTX in parsed JSON format
#'
#' @param key String. User API Key for VORTX.
#' @param job String or List. Can be either a job ID number
#' in string format or parsed JSON in list format,
#' result of organizer or discoverer functions.
#' @param clusternum Integer or String. Can be cluster number (int) or cluster name in format 'cluster-x'.
#' @return List of parsed JSON.
#' @examples
#' mykey <- '1234567890abcefghijkl'
#' myjobid <- '0987654321'
#'
#' myjobname <- 'My job'
#' myjobdesc <- 'This is a job that does job stuff'
#' df <- data.frame(c(1,2,3), c(4,5,6), c(7,8,9))
#' myjob <- vortx_create_job(mykey, df, myjobname, myjobdesc)
#'
#' get_summaryview_raw(mykey, myjob, 1, 1, 10)
get_summaryview_raw <- function(key, job, clusternum){

  # Temporary data
  url <- 'https://api.vortx.io/analyses/summaryview'
  job_body <- list(apikey = key,
                   jobid = get_job_id(job),
                   clusterid = get_cluster_id(clusternum))

  # Function response
  resp <- httr::GET(url, query = job_body)
  summaryview <- httr::content(resp, 'parsed')
  return(summaryview)
}

#' Get Cluster's Summaryview
#'
#' Gets summaryview for a cluster of job in VORTX in list of DataFrame
#'
#' @param key String. User API Key for VORTX.
#' @param job String or List. Can be either a job ID number
#' in string format or parsed JSON in list format,
#' result of organizer or discoverer functions.
#' @param clusternum Integer or String. Can be cluster number (int) or cluster name in format 'cluster-x'.
#' @return List of DataFrames.
#' @examples
#' mykey <- '1234567890abcefghijkl'
#' myjobid <- '0987654321'
#'
#' myjobname <- 'My job'
#' myjobdesc <- 'This is a job that does job stuff'
#' df <- data.frame(c(1,2,3), c(4,5,6), c(7,8,9))
#' myjob <- vortx_create_job(mykey, df, myjobname, myjobdesc)
#'
#' get_summaryview_single(mykey, myjob, 1)
get_summaryview_single <- function(key, job, clusternum){

  summaryview <- get_summaryview_raw(key, job, clusternum)

  options(scipen=999)
  view <- summaryview$vars

  # Get categoricals
  l <- list()
  j <- 1
  for (i in view){
    if (i[[2]] != 'NUMERICAL'){
      name <- i[[1]]
      df <- data.frame(do.call(rbind, i[[3]]))
      df$percentage <- as.numeric(df$percentage)
      df$count <- as.integer(df$count)
      df$name <- as.factor(as.character(df$name))
      names <- row.names(df)
      df <- df[order(-df$percentage), ]
      row.names(df) <- names

      l[[name]] <- df
    }
  }

  # Get numericals
  n <- list()
  for (i in view){
    if (i[[2]] == 'NUMERICAL'){
      var <- i[[1]]
      type <- i[[2]]
      n[[var]] <- cbind(var, type, do.call(cbind, i[[3]]))
    }
  }
  l[['Numericals']] <- do.call(rbind, n)

  return(l)
}

#' Get full Summaryview
#'
#' Gets summaryview of job in VORTX in List of DataFrame format
#'
#' @param key String. User API Key for VORTX.
#' @param job String or List. Can be either a job ID number
#' in string format or parsed JSON in list format,
#' result of organizer or discoverer functions.
#' @return List of DataFrames.
#' @examples
#' mykey <- '1234567890abcefghijkl'
#' myjobid <- '0987654321'
#'
#' myjobname <- 'My job'
#' myjobdesc <- 'This is a job that does job stuff'
#' df <- data.frame(c(1,2,3), c(4,5,6), c(7,8,9))
#' myjob <- vortx_create_job(mykey, df, myjobname, myjobdesc)
#'
#' get_summaryview(mykey, myjob)
get_summaryview <- function(key, job){

  # Get a list of clusters
  hierarchy <- get_hierarchy(key, job)
  clusters <- as.character(hierarchy[-1,1])

  cluster_view <- list()
  for (i in clusters){
    name <- i
    cluster_name <- gsub('-', '', name)
    cluster_view[[cluster_name]] <- get_summaryview_single(key, job, name)
  }
  return(cluster_view)
}
