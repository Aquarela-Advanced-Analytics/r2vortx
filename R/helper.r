# Helper function to create a list of columns
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

# Helper function to get job ID
get_job_id <- function(job){
  if(typeof(job) == 'list'){
    return(as.character(job$job$id))
  } else if (typeof(job) == 'character'){
    return(job)
  } else {
    return(print('There is no job ID here'))
  }
}

# Helper function to get Cluster ID
get_cluster_id <- function(num){
  if (nchar(num) > 1){
    return(num)
  }
  id <- paste('cluster', '-', as.character(num), sep='')
  return(id)
}
