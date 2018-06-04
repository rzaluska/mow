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

    transformation_parameters[[i]] = list(col_i, all_vals)
  }

  return(transformation_parameters)
}


#' binary_coding.apply - add new binary values columns to data
#'
#' @param data
#' @param transformation_parameters - paramets obtained from binary_coding.prepare
#'
#' @return data with added binary columns
#' @export
#'
#' @examples
#' t = binary_coding.prepare(iris, c(5))
#' binary_coding.apply(iris, t) -> add 2 new colums binary representing 3 classes
binary_coding.apply <- function(data, transformation_parameters) {
  data_copy = cbind(data)
  for (i in 1:length(transformation_parameters)) {
    col_i = transformation_parameters[[i]][[1]]
    all_vals = transformation_parameters[[i]][[2]]
    data_copy[all_vals] = 0
    for (n in all_vals) {
      data_copy[data[, col_i] == n, n] = 1
    }
  }

  return(data_copy)
}

# testing
letters = iris[1:5, 1:2]
letters[1,1] = "A"
letters[2,1] = "A"
letters[3,1] = "B"
letters[4,1] = "C"
letters[5,1] = "D"
p = binary_coding.prepare(letters, c(1))
after = binary_coding.apply(letters, p)
after
