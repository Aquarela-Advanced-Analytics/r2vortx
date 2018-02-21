#' Get summary of numericals and categoricals
#'
#' @param df data.frame to be summarized
#' @return list with summary for Numericals, Categories and individual factors for each category
#' @keywords internal
#' @export
get_summary <- function(df){

  # Set values
  metrics <- list()
  metrics[["Numericals"]] <- data.frame()
  metrics[["Categoricals"]] <- data.frame()
  metrics[["Detailed"]] <- list()

  # Get numerics
  num_vars <- sapply(df, is.numeric)
  nums <- df[ ,num_vars, drop=FALSE]
  cats <- df[, !num_vars, drop=FALSE]

  if (length(nums) > 0){
    # Get metrics for nums
    min <- sapply(nums, function(x) min(x[!is.na(x)]))
    max <- sapply(nums, function(x) max(x[!is.na(x)]))
    mean <- sapply(nums, function(x) mean(x[!is.na(x)]))
    stdev <- sapply(nums, function(x) sd(x[!is.na(x)]))
    names <- names(nums)
    missing1 <- sapply(nums, function(x) sum(is.na(x)))
    metrics[["Numericals"]] <- data.frame(Variables=names, Min=min, Max=max,
                                          Mean=mean, StDev=stdev, Missing=missing1)
  }

  if (length(cats) > 0){
    # Get metrics for not nums
    cats[sapply(cats, is.character)] <- lapply(cats[sapply(cats, is.character)], as.factor)
    total <- sapply(cats, function(x) length(levels(x)))
    largest <- sapply(cats, function(x) paste0(names(table(x))[1], " (",
                                               100 * round(max(table(x)) / sum(table(x)), 4), "%)"))
    smallest <- sapply(cats, function(x) paste0(names(table(x))[length(table(x))], " (",
                                                100 * round(min(table(x)) / sum(table(x)), 4), "%)"))
    missing2 <- sapply(cats, function(x) sum(is.na(x)))
    metrics[["Categoricals"]] <- data.frame(Variables=names(cats), Total=total,
                                            Most=largest, Least=smallest, Missing=missing2)

    # Get metrics for each factor
    details <- list()
    for (i in 1:length(cats)){
      vars <- data.frame(table(cats[[i]]))[[1]]
      totals <- data.frame(table(cats[[i]]))[[2]]
      percen <- sapply(table(cats[[i]]), function(x) round(100 * x / length(cats[[i]]), 2))
      details[[names(cats)[[i]]]] <- data.frame(Variables=vars, Totals=totals, Percents=percen,
                                                row.names=1:length(vars))
    }
    metrics[["Detailed"]] <- details
  }

  # Return
  return(metrics)
}
