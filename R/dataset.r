#' Get Raw Textview
#'
#' Gets raw textview of job in VORTX in parsed JSON format
#'
#' @param key String. User API Key for VORTX.
#' @param job String or List. Can be either a job ID number
#' in string format or parsed JSON in list format,
#' result of organizer or discoverer functions.
#' @param clusternum Integer or String. Can be cluster number (int) or cluster name in format 'cluster-x'.
#' @param start Integer. Starting row to get info from.
#' @param end Integer. Ending row to get info from.
#' @param vortx_server Choose server to run Vortx. Can be one of "production", "sandbox", "local" or desired URL.
#' @return List of parsed JSON.
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
#' get_textview_raw(mykey, myjob, 1, 1, 10)
#' }
get_textview_raw <- function(key, job, clusternum, start, end, vortx_server="production"){

  # Temporary data
  if (vortx_server == "production") {
    host_url <- "https://api.vortx.io"
  } else if (vortx_server == "sandbox") {
    host_url <- "https://sandbox-api.vortx.io"
  } else if (vortx_server == "local") {
    host_url <- "http://localhost:8080"
  } else {
    host_url <- vortx_server
  }
  url <- paste0(host_url, "/analyses/textview")
  job_body <- list(apikey = key,
                   jobid = get_job_id(job),
                   clusterid = get_cluster_id(clusternum),
                   start = start,
                   end = end)

  # Function response
  resp <- httr::GET(url, query = job_body)
  textview <- httr::content(resp, 'parsed')

  if (httr::status_code(resp) >= 300) {
    stop(print(textview), call. = FALSE)
  }

  return(textview)
}

#' Get dataset for a single cluster
#'
#' Gets dataset for a cluster of job in VORTX in DataFrame format
#'
#' @param key String. User API Key for VORTX.
#' @param job String or List. Can be either a job ID number
#' in string format or parsed JSON in list format,
#' result of organizer or discoverer functions.
#' @param clusternum Integer or String. Can be cluster number (int) or cluster name in format 'cluster-x'.
#' @param start Integer. Starting row to get info from.
#' @param end Integer. Ending row to get info from.
#' @param vortx_server Choose server to run Vortx. Can be one of "production", "sandbox", "local" or desired URL.
#' @return DataFrame of a cluster dataset
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
#' get_dataset_single(mykey, myjob, 1, 1, 10)
#' }
get_dataset_single <- function(key, job, clusternum, start, end, vortx_server="production"){

  # Get raw information
  textview <- get_textview_raw(key, job, clusternum, start, end, vortx_server)

  # Make the dataset
  df <- data.frame(
    matrix(
      data = unlist(textview),
      nrow = length(textview),
      byrow = TRUE
    ),
    stringsAsFactors = FALSE
  )
  names(df) <- unlist(textview[[1]])
  df <- df[-1, ]
  names <- c(1:length(df[,1]))
  row.names(df) <- names

  return(df)

}

#' Get full dataset
#'
#' Gets full dataset of job in VORTX in DataFrame format
#'
#' @param key String. User API Key for VORTX.
#' @param job String or List. Can be either a job ID number
#' in string format or parsed JSON in list format,
#' result of organizer or discoverer functions.
#' @param vortx_server Choose server to run Vortx. Can be one of "production", "sandbox", "local" or desired URL.
#' @return DataFrame of a job dataset
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
#' get_dataset(mykey, myjob)
#' }
get_dataset <- function(key, job, vortx_server="production"){

  # Temporary data
  hierarchy <- get_hierarchy(key, job, vortx_server)
  clusters <- as.character(hierarchy[-1,1])
  size <- as.character(hierarchy[-1, 3])

  # Get dataset
  data <- list()
  i <- 1
  while (i <= length(clusters)){
    name <- clusters[i]
    end <- size[i]
    data[[i]] <- get_dataset_single(key, job, name, '1', end, vortx_server)
    i <- i + 1
  }
  df <- do.call(rbind.data.frame, data)
  rows <- c(1:length(df[,1]))
  row.names(df) <- rows

  # Adjust type of dataset variables
  summary_raw <- get_summaryview_raw(key, job, 1, vortx_server)
  summary <- data.frame(do.call(rbind, summary_raw$vars))[,-3]
  var_names <- summary[[1]]
  var_types <- summary[[2]]

  i <- 1
  while (i <= length(var_names)){
    name <- var_names[[i]]
    type <- var_types[[i]]
    if (type == 'NUMERICAL'){
      df[,name] <- as.numeric(as.character(df[,name]))
    }
    i <- i + 1
  }
  df[,2] <- as.character(df[,2])
  df[,'clusterId'] <- factor(df[,'clusterId'])

  return(df)

}
