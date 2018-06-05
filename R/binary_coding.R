#' binary_coding.prepare - collect unique values form data columns in order to establish binary coding
#'
#' @param data
#' @param columnIndices - columns selected for transformation
#'
#' @return - list of unique values per column
#' @export
#'
#' @examples
binary_coding.prepare <- function(data, columnIndices) {
  transformation_parameters = list()
  for (i in 1:length(columnIndices)) {
    col_i = columnIndices[i]
    all_vals = names(table(data[, col_i]))
    all_vals = all_vals[1:(length(all_vals) - 1)]
    col_name = colnames(data)[col_i]

    transformation_parameters[[i]] = list(col_i, all_vals, col_name)
  }

  return(transformation_parameters)
}


#' binary_coding.apply - add new binary values columns to data
#'
#' @param data
#' @param transformation_parameters - paramets obtained from binary_coding.prepare
#' @param replace - replace exisiting columns with new ones (default False)
#'
#' @return data with added binary columns
#' @export
#'
#' @examples
#' t = binary_coding.prepare(iris, c(5))
#' binary_coding.apply(iris, t) -> add 2 new colums binary representing 3 classes
binary_coding.apply <- function(data, transformation_parameters, replace=F) {
  data_copy = cbind(data)
  for (i in 1:length(transformation_parameters)) {
    col_i = transformation_parameters[[i]][[1]]
    all_vals = transformation_parameters[[i]][[2]]
    col_name = transformation_parameters[[i]][[3]]
    data_copy[paste(col_name, all_vals, sep='')] = as.integer(0)
    for (n in all_vals) {
      data_copy[data[, col_i] == n, paste(col_name, n, sep='')] = as.integer(1)
    }

    if (replace) {
      data_copy = data_copy[, !(colnames(data_copy) %in% c(col_name))]
    }
  }


  return(data_copy)
}
