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
#' mykey <- '1234567890abcefghijkl'
#' myjobid <- '0987654321'
#'
#' myjobname <- 'My job'
#' myjobdesc <- 'This is a job that does job stuff'
#' df <- data.frame(c(1,2,3), c(4,5,6), c(7,8,9))
#' myjob <- create_job(mykey, df, myjobname, myjobdesc)
#'
#' start_organizer(mykey, myjob, 'WhateverCol')
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
#' @param data DataFrame. Data to be created as a job.
#' @param jobname String. Title of job to be created.
#' @param jobdesc String. Description of job to be created. Optional. Default NULL.
#' @param ignoredcols String or Vector of strings. Optional name of columns to be ignored. Default is NULL.
#' @param id Integer or String. This will be checked as possible ID. Default is 1.
#' @return Job. Parsed content of API request, containing job information, such as job ID, used in other functions.
#' @export
#' @examples
#' mykey <- '1234567890abcefghijkl'
#' myjobname <- 'My job'
#' myjobdesc <- 'This is a job that does job stuff'
#' df <- r2vortx::wine
#'
#' vortx_organizer(mykey, df, myjobname, myjobdesc, 'Ash')
vortx_organizer <- function(key, data, jobname, jobdesc=NULL, ignoredcols=NULL, id=1){

  # Make sure ID column is correct
  data <- get_id_column(data, id)

  # Check for possible IDish columns and/or columns with one value and ignore them
  pos_ids <- c()
  for(name in names(data)){
    if(is_idish(data, name)){
      pos_ids <- c(pos_ids, name)
    }
  }
  if(length(pos_ids) >= 2){
    ignoredcols <- c(ignoredcols, pos_ids[2:length(pos_ids)])
  }

  # Check for columns with one value and ignore them
  useless_cols <- c()
  for(name in names(data)){
    if(length(table(data[[name]])) == 1){
      useless_cols <- c(useless_cols, name)
    }
  }
  if(length(useless_cols) >= 1){
    ignoredcols <- c(ignoredcols, useless_cols)
  }

  # Create job and run organizer
  job <- create_job(key, data, jobname, jobdesc)
  job_id <- get_job_id(job)
  organizer <- start_organizer(key, job_id, ignoredcols)

  return(organizer)
}
