#' Get Raw Metrics of Clusters
#'
#' Gets metrics of clusters of job in VORTX in parsed JSON format.
#'
#' @param key String. User API Key for VORTX.
#' @param job String or List. Can be either a job ID number
#' in string format or parsed JSON in list format,
#' result of organizer or discoverer functions.
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
#' get_metrics_raw(mykey, myjob)
get_metrics_raw <- function(key, job){

  # Temporary data
  url <- 'https://api.vortx.io/analyses/metrics'
  job_body <- list(apikey = key,
                   jobid = get_job_id(job))

  # Function response
  resp <- httr::GET(url, query = job_body)
  metrics <- httr::content(resp, 'parsed')
  return(metrics)
}

#' Get Metrics of Clusters
#'
#' Gets metrics of clusters of job in VORTX in DataFrame format
#'
#' @param key String. User API Key for VORTX.
#' @param job String or List. Can be either a job ID number
#' in string format or parsed JSON in list format,
#' result of organizer or discoverer functions.
#' @return DataFrame.
#' @examples
#' mykey <- '1234567890abcefghijkl'
#' myjobid <- '0987654321'
#'
#' myjobname <- 'My job'
#' myjobdesc <- 'This is a job that does job stuff'
#' df <- data.frame(c(1,2,3), c(4,5,6), c(7,8,9))
#' myjob <- vortx_create_job(mykey, df, myjobname, myjobdesc)
#'
#' get_metrics(mykey, myjob)
get_metrics <- function(key, job){

  metrics <- get_metrics_raw(key, job)

  df <- data.frame(do.call(rbind, metrics$dimSharpness))
  df$sharpness <- as.numeric(df$sharpness)
  df$id <- as.character(df$id)
  df$dead <- as.factor(as.character(df$dead))
  names <- row.names(df)
  df <- df[order(-df$sharpness), ]
  row.names(df) <- names
  df[,-3]
  return(df)
}
