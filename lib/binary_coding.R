#' prepare_transformation
#'
#' @param data 
#' @param columnIndices - columns selected for transformation
#'
#' @return - list of unique values per column
#' @export
#'
#' @examples
prepare_transformation <- function(data, columnIndices) {
  transformation_parameters = list()
  for (i in 1:length(columnIndices)) {
    col_i = columnIndices[i]
    all_vals = names(table(data[, col_i]))
    all_vals = all_vals[1:(length(all_vals) - 1)]
    
    transformation_parameters[[i]] = list(col_i, all_vals)
  }
  
  return(transformation_parameters)
}


#' apply_transformation
#'
#' @param data 
#' @param transformation_parameters - paramets obtained from prepare_transformation
#'
#' @return
#' @export
#'
#' @examples
#' t = prepare_transformation(iris, c(5))
#' apply_transformation(iris, t) -> add 2 new colums binary representing 3 classes
apply_transformation <- function(data, transformation_parameters) {
  data_copy = data
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
p = prepare_transformation(letters, c(1))
after = apply_transformation(letters, p)
after
