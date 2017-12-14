#' Helper Get Column Name
#'
#' Helper function that gets format of columns to be accepted in VORTX.
#'
#' @param col String or Vector of Strings. Cols to be formatted. Can be
#' the name of a column or a vector with names of columns.
#' @return String in format '["col1", "col2", "col3"]'
#' @keywords internal
#' @examples
#' \dontrun{
#' cols <- c('col1', 'col2')
#' col <- 'col3'
#'
#' get_col(cols)
#'
#' get_col(col)
#' }
get_col <- function(col){
  # (vector) -> string
  # Input a character or vector of named columns
  # Output a string in format ["col1", "col2", "col3"]

  begin <- '['
  string <- ''
  i <- 1

  if (length(col) == 1){
    return(paste('[', '"', col, '"', ']', sep=''))
  } else if(length(col) == 0){
    return(NULL)
  }

  col <- unique(col)
  while (i != length(col)+1){
    item <- col[i]

    if (i != length(col)){
      string <- paste(string, '"', item, '"', ', ', sep='')
    } else {
      string <- paste(string, '"', item, '"', sep='')
    }

    i <- i + 1
  }
  end <- paste(begin, string, ']', sep='')

  return(end)
}

#' Helper Get Job ID
#'
#' Helper function that gets format of job ID to be accepted in VORTX.
#'
#' @param job String, Numeric or List of parsed JSON. Can be either the job ID itself
#' in String or Numeric format or a parsed JSON (result of a vortx_create_job)
#' of a job.
#' @return String of job ID
#' @keywords internal
#' @examples
#' \dontrun{
#' jobid <- '1234567890'
#' job <- vortx_create_job(...)
#'
#' get_job_id(jobid)
#'
#' get_job_id(job)
#' }
get_job_id <- function(job){
  if(class(job) == 'list'){
    if ('id' %in% names(job)){
      return(as.character(job$id))
    } else {
      return(as.character(job$job$id))
    }
  } else if (class(job) == 'character'){
    return(job)
  } else if (class(job) == 'numeric'){
    return(as.character(job))
  } else {
    return(print('There is no job ID here'))
  }
}

#' Helper Get Cluster ID
#'
#' Helper function that gets format of Cluster ID to be accepted in VORTX.
#'
#' @param num Integer or String. Can be either the number of the Cluster or
#' the name of cluster in format 'cluster-1'
#' @return String of cluster-id in format 'cluster-1'
#' @keywords internal
#' @examples
#' \dontrun{
#' clusterid <- 'cluster-1'
#'
#' get_cluster_id(clusterid)
#'
#' get_cluster_id(1)
#' }
get_cluster_id <- function(num){
  if (nchar(num) > 1){
    return(num)
  }
  id <- paste('cluster', '-', as.character(num), sep='')
  return(id)
}

#' Add Empty Cell
#'
#' Helper function to add an empty cell in a list
#'
#' @param list List to be used.
#' @param item String to be added as name of item in list.
#' @param index Integer. Index of item to be added.
#' @return List. New list with item added with '-' as value.
#' @keywords internal
add_empty <- function(list, item, index){
  list[[item]] <- '-'
  len <- length(list)
  value <- list[[len]]
  name <- names(list)[[len]]

  # Start loop
  i <- len
  while(index < i){
    list[[i]] <- list[[i-1]]
    names(list)[[i]] <- names(list)[[i-1]]
    i <- i - 1
  }
  list[[index]] <- value
  names(list)[[index]] <- name
  return(list)
}

#' Is IDish
#'
#' Check if column has same number of unique elements as number of rows
#'
#' @param data DataFrame with column to be checked.
#' @param col Integer or String. Column to be checked.
#' @return Boolean
is_idish <- function(data, col){
  uniques <- length(table(data[[col]]))
  col_len <- length(data[[col]])
  return(uniques == col_len)
}

#' Get ID Column
#'
#' Defines which column will be used as ID in the dataset and puts it first named as 'id'
#'
#' @param data DataFrame to be checked
#' @param id Integer or String. This will be checked as possible ID. Default is 1.
#' @return DataFrame with ID column first.
#' @examples
#' \dontrun{
#' df <- r2vortx::wine
#' get_id_column(df)
#' }
get_id_column <- function(data, id=1){
  # Check for possible ID columns for later use
  pos_ids <- c()
  for(name in names(data)){
    if(is_idish(data, name)){
      pos_ids <- c(pos_ids, name)
    }
  }

  # Define first column for later use
  first <- names(data)[1]

  # If user selected specific ID, use it.
  if(id != 1 & is_idish(data, id)){
    n <- 1
    len <- length(data)

    if(is.numeric(id)){
      index <- id
      v <- index

    } else if(is.character(id)){
      col_regex <- paste('^', id, '$', sep='')
      index <- grep(col_regex, names(data))
      v <- index
    }

    while(n <= len){
      if(n != index){
        v <- c(v, n)
      }
      n <- n + 1
    }
    data <- data[,v]
    names(data)[1] <- 'id'

    # Else, check if first column isn't called ID
  } else if(first != 'id' & first != 'ID' & first != 'Id' & first != 'iD'){

    # If there's a possible ID column, move it to first and rename it
    if(!is.null(pos_ids)){
      n <- 1
      len <- length(data)
      col_regex <- paste('^', pos_ids[1], '$', sep='')
      index <- grep(col_regex, names(data))
      v <- index
      while(n <= len){
        if(n != index){
          v <- c(v, n)
        }
        n <- n + 1
      }
      data <- data[,v]
      names(data)[1] <- 'id'

      # If there's NOT a possible ID column, create one
    } else if(is.null(pos_ids)){
      n <- 1
      data$id <- 1:length(data[[1]])
      len <- length(data)
      data <- data[ ,c(len, 1:(len-1))]
    }

    # If it IS called ID, let's use it
  } else if(first == 'id' | first == 'ID' | first == 'Id' | first == 'iD'){
    names(data)[1] <- 'id'
  }

  return(data)
}


#' Dataset of Wine
#'
#' A dataset containing characteristics of wines.
#'
#' @name wine
#' @docType data
#' @format A data frame with 178 rows and 14 variables.
'wine'
