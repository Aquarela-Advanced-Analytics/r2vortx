#' Upload data to local OpenRefine
#'
#' Uploads a DataFrame to an OpenRefine at the same machine
#'
#' @param data DataFrame to be uploaded
#' @param name String. Name of project to be created. Default is NULL (Untitled)
#' @export
#' @examples
#' \dontrun{
#' df <- r2vortx::wine
#' refine_push(df, 'Nameless Project')
#' }
refine_push <- function(data, name = NULL){

  # Set temporary data file
  temp <- tempfile(pattern = 'refineproject', tmpdir = tempdir(), fileext = '.csv')
  utils::write.csv(data, temp, row.names = FALSE)
  proj_csv <- httr::upload_file(temp, 'text/csv')

  # Set parameters for request
  url <- 'http://localhost:3333/command/core/create-project-from-upload'
  body <- list('project-file' = proj_csv,
               'project-name' = project.name)

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

#' Downloads csv file from OpenRefine
#'
#' Extracts a dataset in OpenRefine to R as CSV and loaded as a DataFrame
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
refine_pull <- function(id=NULL, name=NULL, format='csv', encoding='UTF-8'){

  # Set parameters for request
  url <- 'http://localhost:3333/command/core/export-rows'
  engine <- list(facets = "", mode = "row-based")
  body <- list(project = id,
               engine = engine,
               format = format)

  # Make request
  resp <- htth::POST(url = url,
                     body = body,
                     encode = 'form')
  parsed <- httr::content(resp, 'text', 'text/csv', encoding)

  if (httr::status_code(resp) >= 300) {
    stop(print(parsed), call. = FALSE)
  }



  # Read dataset into R
  df <- readr::read_csv(parsed, col_names = TRUE, col_types = NULL)
  return(df)

}

get_refine_id <- function(project_name, project_id){

  # If nothing is given, return error
  if (is.null(project_id) & is.null(project_name)){
    stop('Please specify ID or name of OpenRefine project.')

  # If only name is given, get ID from metadata
  } else if (is.null(project_id) & !is.null(project_name)){
    cont <- httr::content(httr::GET('http://localhost:3333/command/core/get-all-project-metadata'))
    projects <- cont$projects
    matches <- list()
    modifieds <- list()

    for (projid in names(projects)){
      if (projects[[projid]]$name == project_name){
        matches[[projid]] <- projects[[projid]]$name
        modifieds[[projid]] <- projects[[projid]]$modified
      }
    }

    # If there's no matching name, return error
    if (length(matches) == 0){
      stop('There is no matching name.')

    # If there's only one match, return ID
    } else if (length(matches) == 1){
      return(names(matches)[1])

    # If there's more than one match, return latest
    } else if (length(matches) > 1){
      projects_matrix <- matrix(c(matches, modifieds), ncol=2, byrow=TRUE)
      projects_matrix <- matrix[order(matrix[,2]), ]
      return(projects_matrix[1,1])
    }

  # Finally, if ID has been given, return ID
  } else {
    return(project_id)
  }
}

# refine_list <- function(){
#
#   resp <- httr::GET('http://localhost:3333/command/core/get-all-project-metadata')
#   parsed <- httr::content(resp, 'parsed')
#
# }
