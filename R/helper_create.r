#' Get file from different source if source is not NULL
#'
#' @param data Depends on source. DataFrame for R, virtually anything that
#' identifies a googlesheet for 'googlesheets' or path for 'excel'.
#' @param source String defining source. May contain 'excel' or 'googlesheets'. Default is r.
#' Use 'googlesheets_new' for new user.
#' @param sheet number or name of sheet to be imported from source. Default NULL for first.
#' @return DataFrame to be sent to vortx
#' @keywords internal
get_source <- function(data, source='r', sheet=NULL){

  # Set file from Excel xlsx
  if (source == 'excel'){
    file <- readxl::read_excel(path=data, sheet=sheet, col_types='text')

  # Set file from Googlesheets
  } else if (source == 'googlesheets' | source == 'googlesheets_new'){
    if (source == 'googlesheets_new'){
      googlesheets::gs_auth(new_user=TRUE)
    }
    reg_sheet <- googlesheets::gs_title(data)
    if (is.null(sheet)){
      sheet <- 1
    }
    file <- googlesheets::gs_read(ss=reg_sheet, ws=sheet)
  # Set file from R
  } else {
    file <- data
  }
  return(file)
}

#' Get columns that should be ignored in VORTX
#'
#' @param data data to be tested
#' @return vector with ignored columns
#' @keywords internal
get_ignored <- function(data){
  # Check for possible IDish columns and/or columns with one value and ignore them
  ignoredcols <- c()
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
  return(ignoredcols)
}

#' Ignore variables
#'
#' @param data data with variables to ignore
#' @param ignoredcols character vector with names of variables to be ignored
#' @return new data with renamed variables
#' @keywords internal
rename_ignored <- function(data, ignoredcols) {
  old_names <- new_names <- names(data)
  new_names[old_names %in% ignoredcols] <- gsub("^(.*)", "!\\1", ignoredcols)
  names(data) <- new_names
  return(data)
}

#' Is IDish
#'
#' Check if column has same number of unique elements as number of rows
#'
#' @param data DataFrame with column to be checked.
#' @param col Integer or String. Column to be checked.
#' @return Boolean
#' @keywords internal
is_idish <- function(data, col){
  uniques <- length(table(data[[col]]))
  col_len <- length(data[[col]])
  return(uniques == col_len & !is.numeric(data[[col]]))
}

#' Get ID Column
#'
#' Defines which column will be used as ID in the dataset and puts it first named as 'id'
#'
#' @param data DataFrame to be checked
#' @param id Integer or String. This will be checked as possible ID. Default is 1.
#' @return DataFrame with ID column first.
#' @keywords internal
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

#' Move Target column to first after ID
#'
#' @return DataFrame with target column after ID
#' @keywords internal
get_target_column <- function(data, target){

  # Check if column exists
  if (!target %in% names(data)) stop("There is no such Target column", call. = FALSE)

  # Move column
  index <- grep(target, names(data))
  col_indexes <- 1:length(data)
  col_order <- c(1, index, col_indexes[!col_indexes %in% c(1, index)])
  data <- data[ , col_order]
  return(data)
}

#' Delete column with only one value
#'
#' @param data data to remove constant
#' @return List with DataFrame with removed column and the name of the column
#' @keywords internal
remove_constant <- function(data){

  # Get constant column(s)
  constant <- sapply(data, function(x) 1 == length(table(x)))

  # If there is one, delete it and save its name
  if (sum(constant) >= 1){
    col_names <- names(constant[constant])
    col_names <- paste0("[", col_names, "]", collapse='')
    data <- data[!constant]
    cat(paste0("Variable(s) removed: ", col_names))
  } else col_names <- c()

  return(list(Dataset=data, Names=col_names))
}
