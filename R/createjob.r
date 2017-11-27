#' Create job
#'
#' Creates a job in VORTX
#'
#' @param key String. User API Key for VORTX.
#' @param data DataFrame. Data to be created as a job.
#' @param jobname String. Title of job to be created.
#' @param jobdesc String. Description of job to be created. Optional. Default NULL.
#' @return Job. Parsed content of API request, containing job information, such as job ID, used in other functions.
#' @examples
#' mykey <- '1234567890abcefghijkl'
#' myjobname <- 'My job'
#' myjobdesc <- 'This is a job that does job stuff'
#' df <- data.frame(c(1,2,3), c(4,5,6), c(7,8,9))
#'
#' vortx_create_job(mykey, df, myjobname, myjobdesc)
vortx_create_job <- function(key, data, jobname, jobdesc=NULL){

  # Temporary data
  temp <- tempfile(pattern = 'vortxjob', tmpdir = tempdir(), fileext = '.csv')
  write.csv(data, temp, row.names = FALSE)
  job_csv <- upload_file(temp, 'text/csv')

  # Body of request
  url <- 'https://api.vortx.io/jobs/create'
  job_body <- list(apikey = key,
                   name = jobname,
                   description = jobdesc,
                   csv = job_csv)

  # Function response
  resp <- POST(url, body = job_body,
               encode = 'multipart',
               verbose())

  parsed <- content(resp, 'parsed') # get response

  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE) # check for json response
  }

  if (status_code(resp) >= 300) {
    stop(print(parsed), call. = FALSE)
  }

  return(parsed)
}
