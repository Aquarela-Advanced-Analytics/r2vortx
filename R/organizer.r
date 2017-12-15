#' Start Organizer
#'
#' Runs Organizer from a created Job in VORTX
#'
#' @param key String. User API Key for VORTX.
#' @param job String or List. Can be either a job ID number
#' in string format or parsed JSON in list format,
#' result of organizer or discoverer functions.
#' @param ignoredcols String or Vector of strings. Optional name of columns to be ignored. Default is NULL.
#' @return Job. Parsed content of API request, containing job information, such as job ID, used in other functions.
#' @examples
#' \dontrun{
#' mykey <- '1234567890abcefghijkl'
#' myjobid <- '0987654321'
#'
#' myjobname <- 'My job'
#' myjobdesc <- 'This is a job that does job stuff'
#' df <- data.frame(c(1,2,3), c(4,5,6), c(7,8,9))
#' myjob <- create_job(mykey, df, myjobname, myjobdesc)
#'
#' start_organizer(mykey, myjob, 'WhateverCol')
#' }
start_organizer <- function(key, job, ignoredcols=NULL){
  # job can be either a list containing job data
  # or a character with jobid

  # Temporary data
  job_id <- get_job_id(job)
  url <- 'https://api.vortx.io/jobs/start'
  job_body <- list(apikey = key,
                   jobid = job_id,
                   ignoredcols = get_col(ignoredcols))

  # Function response
  resp <- httr::POST(url, body = job_body,
               encode = 'multipart',
               httr::verbose())

  parsed <- httr::content(resp, 'parsed')

  if (httr::status_code(resp) >= 300) {
    stop(print(parsed), call. = FALSE)
  }

  return(parsed)
}

#' Run Organizer from Scratch
#'
#' Creates a job and runs Organizer
#'
#' @param key String. User API Key for VORTX.
#' @param data Data to be created as a job. If no source is defined, it should be a DataFrame.
#' If source is 'googlesheets', could be name, ID or any information that refers to it.
#' If source is 'excel', it should be the path to file.
#' @param jobname String. Title of job to be created.
#' @param jobdesc String. Description of job to be created. Optional. Default NULL.
#' @param ignoredcols String or Vector of strings. Optional name of columns to be ignored. Default is NULL.
#' @param id Integer or String. This will be checked as possible ID. Default is 1.
#' @param source String defining source. May contain 'excel' or 'googlesheets'. Default NULL for R.
#' @param sheet String with number or name of sheet to be imported from source. Default NULL for first. Unnecessary for R.
#' @return Job. Parsed content of API request, containing job information, such as job ID, used in other functions.
#' @export
#' @examples
#' \dontrun{
#' mykey <- '1234567890abcefghijkl'
#' myjobname <- 'My job'
#' myjobdesc <- 'This is a job that does job stuff'
#' df <- r2vortx::wine
#'
#' vortx_organizer(mykey, df, myjobname, myjobdesc, 'Ash')
#' }
vortx_organizer <- function(key, data, jobname, jobdesc=NULL, ignoredcols=NULL, id=1, source=NULL, sheet=NULL){

  # Check source
  file <- get_source(data, source, sheet)

  # Make sure ID column is correct
  file <- get_id_column(file, id)

  # Get ignored columns
  ignored <- get_ignored()
  ignoredcols <- c(ignoredcols, ignored)

  # Create job and run organizer
  job <- create_job(key, file, jobname, jobdesc)
  job_id <- get_job_id(job)
  organizer <- start_organizer(key, job_id, ignoredcols)

  return(organizer)
}
