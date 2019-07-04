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
refine_push <- function(data, name=NULL, base_url='http://localhost:3333'){

  # Set temporary data file
  temp <- tempfile(pattern = 'refineproject', tmpdir = tempdir(), fileext = '.csv')
  utils::write.csv(data, temp, row.names = FALSE)
  proj_csv <- httr::upload_file(temp, 'text/csv')

  # Set parameters for request
  base_url = sub('/$', '', base_url)
  url <- paste0(base_url, '/command/core/create-project-from-upload')
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
#' @param name String. Name of project in OpenRefine. Either name or ID must be given.
#' @param id String. ID of project in OpenRefine. Obtainable by clicking 'About' in OpenRefine. Either name or ID must be given.
#' @param encoding String. Encoding of uploaded file. Default is UTF-8.
#' @return DataFrame of dataset in OpenRefine
#' @export
#' @examples
#' \dontrun{
#' df <- refine_pull('1622372310646')
#' }
refine_pull <- function(name=NULL, id=NULL, encoding='UTF-8', base_url='http://localhost:3333'){

  # Set parameters for request
  base_url = sub('/$', '', base_url)
  url <- paste0(base_url, '/command/core/export-rows')
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
refine_list <- function(base_url='http://localhost:3333'){

  base_url = sub('/$', '', base_url)
  url <- paste0(base_url, '/command/core/get-all-project-metadata')
  cont <- httr::content(httr::GET(url), as = 'parsed')

  projects <- data.frame(do.call(rbind, cont$projects))
  projects$id <- row.names(projects)
  # projects <- projects[, c('ID', 'name', 'modified', 'rowCount')]
  projects <- projects[, c('id', 'name', 'modified', 'customMetadata')]
  names(projects) <- c('id', 'name', 'modified', 'custom_metadata')
  projects$id <- as.character(projects$id)
  projects$name <- as.character(projects$name)
  projects$modified <- as.character(projects$modified)
  # projects$Rows <- as.integer(projects$rows)
  projects <- projects[order(projects$modified, decreasing=TRUE), ]
  row.names(projects) <- c(1:length(projects[[1]]))

  return(projects)

}

#' Delete OpenRefine project
#'
#' Kills project in OpenRefine
#'
#' @param name String. Name of project in OpenRefine. Either name or ID must be given.
#' @param id String. ID of project in OpenRefine. Obtainable by clicking 'About' in OpenRefine. Either name or ID must be given.
#' @export
refine_kill <- function(name=NULL, id=NULL, base_url='http://localhost:3333'){

  # Set parameters
  base_url = sub('/$', '', base_url)
  url <- paste0(base_url, '/command/core/delete-project')
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

#' Apply (mimes) changes from existent project in OpenRefine to R data
#'
#' @param data DataFrame to be mimed
#' @param mime.name String with name of project that contains the changes. Either name or ID must be given.
#' @param mime.id String with ID of project that contains the changes. Either name or ID must be given.
#' @export
#' @examples
#' \dontrun{
#' refine_mime(df, 'diamonds')
#' }
refine_mime <- function(data, mime.name=NULL, mime.id=NULL, base_url='http://localhost:3333'){

  # Send data
  refine_push(data, 'Temporary')

  # Get operations from mime
  mime_id <- get_refine_id(mime.name, mime.id, base_url = base_url)
  base_url = sub('/$', '', base_url)
  operations_url <- paste0(base_url, '/command/core/get-operations')
  operations <- httr::GET(operations_url,
                          query=list(project=mime_id))
  operations <- httr::content(operations, 'text')
  operations <- jsonlite::fromJSON(operations)
  operations <- jsonlite::toJSON(operations$entries$operation)

  # Apply operations on new data
  new_id <- get_refine_id('Temporary', base_url = base_url)
  apply_url <- paste0(base_url, '/command/core/apply-operations?')
  httr::POST(url = apply_url,
             body = list(project=new_id, operations=operations),
             encode='form')
  message('Changes being applied...')

  # Get data back and delete project
  new_data <- refine_pull(id=new_id, base_url = base_url)
  refine_kill(id=new_id, base_url = base_url)
  return(new_data)

}

#' Gets ID from project in OpenRefine
#'
#' @param project_name String with project Name
#' @param project_id String with project ID
#' @return String with project ID
#' @keywords internal
get_refine_id <- function(project_name=NULL, project_id=NULL, base_url='localhost:3333'){

  # If nothing is given, return error
  if (is.null(project_id) & is.null(project_name)){
    stop('Please specify ID or name of OpenRefine project.')

  # If only name is given, get ID from metadata
  } else if (is.null(project_id) & !is.null(project_name)){
    proj_list <- refine_list(base_url = base_url)

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
