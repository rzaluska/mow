#' imputation.prepare - compute mean, median or mode for selected columns, this values can be used to fill missing values
#'
#' @param data
#' @param columnIndices - select columns you want to process
#' @param columnOptions - select type of data inputation schema per column. One of:
#' - 'average'
#' - 'median'
#' - 'mode'
#'
#' @return
#' @export
#'
#' @examples
#' iris_c = iris
#' fil = seq(from=1, to=150, by=2)
#' iris_c[fil, 1:2] = NA
#' iris_c[fil, 5] = NA
#' p = inputation.prepare(iris_c, c(1, 2, 5),  c("average", "median", "mode"))
imputation.prepare <- function(data, columnIndices, columnOptions) {
  transformation_parameters = list()
  for (i in 1:length(columnIndices)) {
     col_i = columnIndices[i]
     option = columnOptions[i]
     val = NA
     not_NA = data[!is.na(data[, col_i]), col_i]
     if (option == "average") {
      val = mean(not_NA)
     } else if (option == "median") {
      val = median(not_NA)
     } else if (option == "mode") {
      val = names(sort(table(not_NA), decreasing=T))[1]
     } else {
       stop("Wrong option: ", option, ", for column index ", col_i, sep="")
     }
     transformation_parameters[[i]] = list(col_i, val)
  }

  return(transformation_parameters)
}



#' imputation.apply - fill data gaps (NA valuse) with values computed by inputation.prepare
#'
#' @param data
#' @param transformation_parameters - paramets obtainded from imputation.prepare function
#' @param mark_artificial_vals - for all columns with missing values add new column and mark artificial values in it (default False)
#'
#' @return data with NA values set based on transformation_parameters
#' @export
#'
#' @examples
#' iris_c = iris
#' fil = seq(from=1, to=150, by=2)
#' iris_c[fil, 1:2] = NA
#' p = inputation.prepare(iris_c, c(1, 2),  c("average", "median"))
#' after1 = inputation.apply(iris_c, p)
imputation.apply <- function(data, transformation_parameters, mark_artificial_vals = F) {
  data_copy = as.data.frame(data)
  for (i in 1:length(transformation_parameters)) {
    col_i = transformation_parameters[[i]][[1]]
    val = transformation_parameters[[i]][[2]]
    data_copy[which(is.na(data[, col_i])), col_i] = val
    if (mark_artificial_vals) {
      col_name = colnames(data_copy)[col_i]
      data_copy[which(!is.na(data[, col_i])), paste(col_name, "a", sep='')] = F
      data_copy[which(is.na(data[, col_i])), paste(col_name, "a", sep='')] = T
    }
  }

  return(data_copy)
}
