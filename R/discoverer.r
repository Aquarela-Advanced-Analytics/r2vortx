#' Start Discoverer
#'
#' Runs Discoverer from a created Job in VORTX
#'
#' @param key String. User API Key for VORTX.
#' @param job String or List. Can be either a job ID number
#' in string format or parsed JSON in list format,
#' result of organizer or discoverer functions.
#' @param target String. Name of column to be used as Target.
#' @param ignoredcols String or Vector of strings. Optional name of columns to be ignored. Default is NULL.
#' @return Job. Parsed content of API request, containing job information, such as job ID, used in other functions.
#' @examples
#' mykey <- '1234567890abcefghijkl'
#' myjobid <- '0987654321'
#' myjob <- vortx_create_job(...)
#'
#' start_discoverer(mykey, myjob, myjobname, myjobdesc)
start_discoverer <- function(key, job, target, ignoredcols=NULL){
  # job can be either a list containing job data
  # or a character with jobid

  # Temporary data
  job_id <- get_job_id(job)
  url <- 'https://api.vortx.io/discoverer/start'
  job_body <- list(apikey = key,
                   jobid = job_id,
                   targetcols = get_col(target),
                   ignoredcols = get_col(ignoredcols))

  # Function response
  resp <- POST(url, body = job_body,
               encode = 'multipart',
               verbose())

  parsed <- content(resp, 'parsed')

  if (status_code(resp) != 200) {
    stop(print(parsed), call. = FALSE)
  }

  return(parsed)
}

#' Run Discoverer from Scratch
#'
#' Creates a job and runs Discoverer
#'
#' @param key String. User API Key for VORTX.
#' @param data DataFrame. Data to be created as a job.
#' @param jobname String. Title of job to be created.
#' @param target String. Name of column to be used as Target.
#' @param jobdesc String. Description of job to be created. Optional. Default NULL.
#' @param ignoredcols String or Vector of strings. Optional name of columns to be ignored. Default is NULL.
#' @return Job. Parsed content of API request, containing job information, such as job ID, used in other functions.
#' @examples
#' mykey <- '1234567890abcefghijkl'
#' myjobname <- 'My job'
#' myjobdesc <- 'This is a job that does job stuff'
#' df <- data.frame(Survived = c(1,0,1), c(4,5,6), WhateverCol = c(7,8,9))
#'
#' vortx_discoverer(mykey, df, myjobname, 'Survived', myjobdesc, 'WhateverCol')
vortx_discoverer <- function(key, data, jobname, target, jobdesc=NULL, ignoredcols=NULL){

  job <- vortx_create_job(key, jobname, data, jobdesc)
  job_id <- get_job_id(job)
  discoverer <- start_discoverer(key, job_id, target, ignoredcols)

  return(discoverer)
}
