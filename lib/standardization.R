#' prepare_transformation
#'
#' @param data 
#' @param columnIndices 
#'
#' @return list of (average, std) values per column selected in columnIndices
#' @export
#'
#' @examples
#' p = prepare_transformation(iris, c(1))
prepare_transformation <- function(data, columnIndices) {
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

#' apply_transformation
#'
#' @param data 
#' @param transformation_parameters - paramets obtained from prepare_transformation
#'
#' @return data with column values replaced by new ones with mean = 0 and std = 1
#' @export
#'
#' @examples
#' p = prepare_transformation(iris, c(1))
#' r = apply_transformation(iris, p)
#' mean(r[,1]) -> 0
#' sd(r[, 1]) -> 1
apply_transformation <- function(data, transformation_parameters) {
  data_copy = data
  for (i in 1:length(transformation_parameters)) {
    col_i = transformation_parameters[[i]][[1]]
    avg = transformation_parameters[[i]][[2]]
    std = transformation_parameters[[i]][[3]]
    data_copy[, col_i] = (data_copy[, col_i] - avg)/std
  }
  
  return(data_copy)
}

# testing
iris_c = iris
fil = seq(from=1, to=150, by=2)
iris_c[fil, 1] = NA
iris_c[1:10,]
p = prepare_transformation(iris_c, c(1))
p[[1]]
after1 = apply_transformation(iris_c, p)
after1[1:10,]