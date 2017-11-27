#' Get Raw Hierarchy of Clusters
#'
#' Gets hierarchy of clusters of job in VORTX in parsed JSON format.
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
#' get_hierarchy_raw(mykey, myjob)
get_hierarchy_raw <- function(key, job){

  # Temporary data
  url <- 'https://api.vortx.io/analyses/hierarchy'
  job_body <- list(apikey = key,
                   jobid = get_job_id(job))

  # Function response
  resp <- httr::GET(url, query = job_body)
  hierarchy <- httr::content(resp, 'parsed')

  return(hierarchy)
}

#' Get Hierarchy of Clusters
#'
#' Gets hierarchy of clusters of job in VORTX in DataFrame format
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
#' get_hierarchy(mykey, myjob)
get_hierarchy <- function(key, job){

  hierarchy <- get_hierarchy_raw(key, job)

  clusters_outer <- hierarchy$children
  clusters_inner <- hierarchy$children[[1]]$children
  df_outer <- data.frame(do.call(rbind, clusters_outer))
  df_inner <- data.frame(do.call(rbind, clusters_inner))
  df_outer$children <- length(clusters_inner)
  df <- rbind(df_outer, df_inner)
  df$id <- factor(as.character(df$id))
  df$name <- factor(as.character(df$name))
  df$size <- as.numeric(as.character(df$size))
  df$representativeness <- as.numeric(as.character(df$representativeness))
  df$sharpness <- as.numeric(as.character(df$sharpness))
  df$children <- as.numeric(as.character(df$children))
  df$classVersion <- as.integer(as.character(df$classVersion))
  df <- df[ ,-7]
  return(df)
}
