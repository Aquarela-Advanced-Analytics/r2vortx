#' Raw list of jobs in VORTX
#'
#' Gets a raw list of jobs in VORTX user's account
#'
#' @param key String. User API Key for VORTX.
#' @param archived String Shows archived jobs. Default: 'true'
#' @param unarchived String Shows unarchived jobs. Default: 'true'
#' @param sandbox Choose TRUE if job is to be sent to sandbox instead
#' @return DataFrame. Available jobs with their respective Job IDs, name and description.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' mykey <- '1234567890abcefghijkl'
#'
#' get_joblist(mykey)
#' }
get_joblist <- function(key, archived='true', unarchived='true', sandbox=FALSE){

  # Temporary data
  if (sandbox) sb <- "sandbox-" else sb <- ""
  url <- paste("https://", sb, "api.vortx.io/jobs/list", sep="")
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
#' @param sandbox Choose TRUE if job is to be sent to sandbox instead
#' @return DataFrame. Available jobs with their respective Job IDs, name and description.
#' @export
#'
#' @examples
#' \dontrun{
#' mykey <- '1234567890abcefghijkl'
#'
#' vortx_joblist(mykey)
#' }
vortx_joblist <- function(key, archived='true', unarchived='true', sandbox=FALSE){

  # Get parsed JSON
  jobs <- get_joblist(key, archived, unarchived, sandbox)

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

  # Selecting which variables to change to which type
  to_numeric <- c('rows', 'cols', 'rawModelVersion', 'estimatedTimeInMin')
  to_factor <- c('jobType', 'status', 'visibility', 'archived')
  to_character <- c(!names(result) %in% c(to_numeric))

  # Change types
  result[,to_numeric] <- lapply(result[,to_numeric], as.numeric)
  result[,to_character] <- lapply(result[,to_character], as.character)
  result[,to_factor] <- lapply(result[,to_factor], factor)

  return(result)
}
