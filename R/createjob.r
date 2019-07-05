#' Create job
#'
#' Creates a job in VORTX
#'
#' @param key String. User API Key for VORTX.
#' @param data DataFrame. Data to be created as a job.
#' @param jobname String. Title of job to be created.
#' @param jobdesc String. Description of job to be created. Optional. Default NULL.
#' @param vortx_server Choose server to run Vortx. Can be one of "production", "sandbox", "local" or desired URL.
#' @return Job. Parsed content of API request, containing job information, such as job ID, used in other functions.
#' @examples
#' \dontrun{
#' mykey <- '1234567890abcefghijkl'
#' myjobname <- 'My job'
#' myjobdesc <- 'This is a job that does job stuff'
#' df <- r2vortx::wine
#'
#' create_job(mykey, df, myjobname, myjobdesc)
#' }
create_job <- function(key, data, jobname, jobdesc=NULL, vortx_server="production"){

  # Write temporary .csv file
  temp <- tempfile(pattern = 'vortxjob', tmpdir = tempdir(), fileext = '.csv')
  utils::write.csv(data, temp, row.names = FALSE, fileEncoding = "UTF-8")
  job_csv <- httr::upload_file(temp, 'text/csv')

  # Body of request
  if (vortx_server == "production") {
    host_url <- "https://api.vortx.io"
  } else if (vortx_server == "sandbox") {
    host_url <- "https://sandbox-api.vortx.io"
  } else if (vortx_server == "local") {
    host_url <- "http://localhost:8080"
  } else {
    host_url <- vortx_server
  }
  url <- paste0(host_url, "/jobs/create")
  job_body <- list(apikey = key,
                   name = jobname,
                   description = jobdesc,
                   csv = job_csv)

  # Function response
  resp <- httr::POST(url, body = job_body,
               encode = 'multipart',
               httr::verbose())

  parsed <- httr::content(resp, 'parsed') # get response

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE) # check for json response
  }

  if (httr::status_code(resp) >= 300) {
    stop(print(parsed), call. = FALSE)
  }

  return(parsed)
}
