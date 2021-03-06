#' standarization.prepare - compute mean and std for columns
#'
#' @param data
#' @param columnIndices
#'
#' @return list of (average, std) values per column selected in columnIndices
#' @export
#'
#' @examples
#' p = standarization.prepare(iris, c(1))
standarization.prepare <- function(data, columnIndices) {
  transformation_parameters = list()
  for (i in 1:length(columnIndices)) {
    col_i = columnIndices[i]
    not_NA = data[!is.na(data[, col_i]), col_i]
    avg = mean(not_NA)
    std = sd(not_NA)
    transformation_parameters[[i]] = list(col_i, avg, std)
  }

  return(transformation_parameters)
}

#' standarization.apply - standarize data in selected columns
#'
#' @param data
#' @param transformation_parameters - paramets obtained from standarization.prepare
#'
#' @return data with column values replaced by new ones with mean = 0 and std = 1
#' @export
#'
#' @examples
#' p = prepare_transformation(iris, c(1))
#' r = apply_transformation(iris, p)
#' mean(r[, 1]) -> 0
#' sd(r[, 1]) -> 1
standarization.apply <- function(data, transformation_parameters) {
  data_copy = data
  for (i in 1:length(transformation_parameters)) {
    col_i = transformation_parameters[[i]][[1]]
    avg = transformation_parameters[[i]][[2]]
    std = transformation_parameters[[i]][[3]]
    data_copy[, col_i] = (data_copy[, col_i] - avg)/std
  }

  return(data_copy)
}
