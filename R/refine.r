#' Push data from R to OpenRefine
#'
#' Pushes data from R to OpenRefine as a CSV
#'
#' @param data DataFrame to be uploaded
#' @param name String. Name of project to be created. Default is NULL (Untitled)
#' @export
#' @examples
#' \dontrun{
#' df <- r2vortx::wine
#' refine_push(df, 'Nameless Project')
#' }
refine_push <- function(data, name=NULL){

  # Set temporary data file
  temp <- tempfile(pattern = 'refineproject', tmpdir = tempdir(), fileext = '.csv')
  utils::write.csv(data, temp, row.names = FALSE)
  proj_csv <- httr::upload_file(temp, 'text/csv')

  # Set parameters for request
  url <- 'http://localhost:3333/command/core/create-project-from-upload'
  body <- list('project-file' = proj_csv,
               'project-name' = name)

  # Make request
  resp <- httr::POST(url = url,
                     body = body,
                     encode = 'multipart')
  parsed <- httr::content(resp, 'parsed')

  if (httr::status_code(resp) >= 300) {
    stop(print(parsed), call. = FALSE)
  } else {
    message('Project created! Check it at http://localhost:3333/')
  }
}

#' Pull data from OpenRefine to R
#'
#' Pulls data from OpenRefine to R as a DataFrame
#'
#' @param id String. ID of project in OpenRefine. Obtainable by clicking 'About' in OpenRefine.
#' @param name String. Name of project in OpenRefine.
#' @param encoding String. Encoding of uploaded file. Default is UTF-8.
#' @return DataFrame of dataset in OpenRefine
#' @export
#' @examples
#' \dontrun{
#' df <- refine_pull('1622372310646')
#' }
refine_pull <- function(name=NULL, id=NULL, encoding='UTF-8'){

  # Set parameters for request
  url <- 'http://localhost:3333/command/core/export-rows'
  engine <- list(facets = "", mode = "row-based")
  project_id <- get_refine_id(name, id)
  body <-    c(project = project_id,
               engine = engine,
               format = 'csv')

  # Make request
  resp <- httr::POST(url = url,
                     body = body,
                     encode = 'form')
  parsed <- httr::content(resp, 'text', 'text/csv', encoding)

  if (httr::status_code(resp) >= 300) {
    stop(print(parsed), call. = FALSE)
  }

  # Read dataset into R
  df <- readr::read_csv(parsed, col_names=TRUE, col_types=NULL)
  return(df)

}

#' Gets list of projects in OpenRefine
#'
#' @export
#' @return DataFrame with projects' ID, Name, Last Modified and Number of Rows
refine_list <- function(){

  cont <- httr::content(httr::GET('http://localhost:3333/command/core/get-all-project-metadata'))

  projects <- data.frame(do.call(rbind, cont$projects))
  projects$ID <- row.names(projects)
  projects <- projects[, c('ID', 'name', 'modified', 'rowCount')]
  names(projects) <- c('ID', 'Name', 'Modified', 'Rows')
  projects$ID <- as.character(projects$ID)
  projects$Name <- as.character(projects$Name)
  projects$Modified <- as.character(projects$Modified)
  projects$Rows <- as.integer(projects$Rows)
  projects <- projects[order(projects$Modified, decreasing=TRUE), ]
  row.names(projects) <- c(1:length(projects[[1]]))

  return(projects)

}

#' Delete OpenRefine project
#'
#' Kills project in OpenRefine
#'
#' @param id String. ID of project in OpenRefine. Obtainable by clicking 'About' in OpenRefine.
#' @param name String. Name of project in OpenRefine.
#' @export
refine_kill <- function(name=NULL, id=NULL){

  # Set parameters
  url <- 'http://localhost:3333/command/core/delete-project'
  project_id <- get_refine_id(name, id)
  body <- list(project = project_id)

  # Make request
  resp <- httr::POST(url = url, body = body, encode = 'form')

  if (httr::status_code(resp) >= 300) {
    stop('Something went wrong...', call. = FALSE)
  } else {
    message('Project deleted!')
  }
}

#' Gets ID from project in OpenRefine
#'
#' @param project_name String with project Name
#' @param project_id String with project ID
#' @return String with project ID
get_refine_id <- function(project_name=NULL, project_id=NULL ){

  # If nothing is given, return error
  if (is.null(project_id) & is.null(project_name)){
    stop('Please specify ID or name of OpenRefine project.')

  # If only name is given, get ID from metadata
  } else if (is.null(project_id) & !is.null(project_name)){
    proj_list <- refine_list()

    # If there's no matching name, return error
    if (length(which(proj_list$Name == project_name)) == 0){
      stop('There is no matching name.')

    # If there's only one match, return ID
    } else if (length(which(proj_list$Name == project_name)) == 1){
      return(proj_list[proj_list$Name == project_name, 'ID'])

    # If there's more than one match, return latest
    } else if (length(which(proj_list$Name == project_name)) > 1){
      message('There is more than one matching name. Returning latest.')
      return(proj_list[proj_list$Name == project_name, 'ID'][[1]])
    }

  # Finally, if ID has been given, return ID
  } else {
    return(project_id)
  }
}
