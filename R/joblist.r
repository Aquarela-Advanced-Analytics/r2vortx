#' Raw list of jobs in VORTX
#'
#' Gets a raw list of jobs in VORTX user's account
#'
#' @param key String. User API Key for VORTX.
#' @param archived String Shows archived jobs. Default: 'true'
#' @param unarchived String Shows unarchived jobs. Default: 'true'
#' @return DataFrame. Available jobs with their respective Job IDs, name and description.
#'
#' @examples
#' mykey <- '1234567890abcefghijkl'
#'
#' get_joblist(mykey)
get_joblist <- function(key, archived='true', unarchived='true'){

  # Temporary data
  url <- 'https://api.vortx.io/jobs/list'
  job_body <- list(apikey = key,
                   archived = archived,
                   unarchived = unarchived)

  # Function response
  resp <- httr::GET(url, query = job_body)
  joblist <- httr::content(resp, 'parsed')

  return(joblist)
}

#' List jobs in VORTX
#'
#' Gets a list of jobs in VORTX user's account
#'
#' @param key String. User API Key for VORTX.
#' @param archived String Shows archived jobs. Default: 'true'
#' @param unarchived String Shows unarchived jobs. Default: 'true'
#' @return DataFrame. Available jobs with their respective Job IDs, name and description.
#'
#' @examples
#' mykey <- '1234567890abcefghijkl'
#'
#' vortx_joblist(mykey)
vortx_joblist <- function(key, archived='true', unarchived='true'){

  # Get parsed JSON
  joblist <- get_joblist(key, archived, unarchived)

  # Turn into something useful
  jobs2 <- list()
  n <- 1
  while(n <= length(jobs)){
    jobs2[[n]] <- jobs[[n]]
    if(!'jobType' %in% names(jobs[[n]])){
      jobs2[[n]] <- add_empty(jobs2[[n]], 'jobType', 2)
    }
    if(!'description' %in% names(jobs[[n]])){
      jobs2[[n]] <- add_empty(jobs2[[n]], 'description', 4)
    }
    if(!'approvedAt' %in% names(jobs[[n]])){
      jobs2[[n]] <- add_empty(jobs2[[n]], 'approvedAt', 8)
      jobs2[[n]] <- add_empty(jobs2[[n]], 'startedAt', 9)
      jobs2[[n]] <- add_empty(jobs2[[n]], 'finishedAt', 10)
    }
    if('errorLog' %in% names(jobs[[n]])){
      jobs2[[n]][['errorLog']] <- NULL
    }
    n <- n + 1
  }
  result <- data.frame(do.call(rbind, jobs2))
}
