# 
#' compute discreatization thresholds - prepare_transformation
#'
#' @param data
#' @param columnIndices
#' @param columnOptions list of options in followin form per column
#' [0] numSlices - number of discrete values to produce
#' [1] type - type of discreatization, one of:
#'     - 'even' - split domain by even slices, where slice size is (max - min)/numSlices
#'     - 'size' - split domain by slices of even numer of examples in them
#'
#' @return list of threshold values of size (numSlices - 1)
#' @export
#'
#' @examples
#' t = prepare_transformation(iris, c(1,2), list(list(5, 'even'),list(5, 'size')))
prepare_transformation <- function(data, columnIndices, columnOptions) {
  tresholds = c()
  for (k in 1:length(columnIndices)) {
    dataColumn = data[,columnIndices[k]]
    opts = columnOptions[[k]]
    numSlices = opts[[1]]
    type = opts[[2]]
    if (! type %in% list('even', 'size')) {
      stop("Wrong type param: ", type, ". Use 'even' or 'size'")
    }
    col_tresholds = c()
    number_of_examples = length(dataColumn)
    if (type == 'even') {
      # we assume that data domain is fully enclosed between max and min values
      slice_size = (max(dataColumn) - min(dataColumn)) / numSlices
      current_index = 0
      currTreshold = min(dataColumn) + slice_size
      
      for (i in 1:(numSlices-1)) {
        col_tresholds[i] = currTreshold
        currTreshold = currTreshold + slice_size
      }
    }
    
    else if (type == 'size') {
      slice_size = as.integer(length(dataColumn) / numSlices)
      sorted = sort(dataColumn)
      current_index = 1
      curr_slice_size = slice_size
      
      for (i in 1:length(dataColumn)) {
        if (curr_slice_size == 0) {
          col_tresholds[current_index] = sorted[i]
          curr_slice_size = slice_size
          current_index = current_index + 1
        }
        else {
          curr_slice_size = curr_slice_size - 1
        }
      }
    }
    tresholds[[k]] = list(c(columnIndices[k]), col_tresholds)
  }
  return(tresholds)
  }
  

#' Title
#'
#' @param dataColumn 
#' @param thresholds values computed by prepare_transformation function
#'
#' @return dataColumn with discrete values
#' @export
#'
#' @examples
#'  t = prepare_transformation(iris, c(1,2), list(list(5, 'even'),list(6, 'size')))
#'  transformed = apply_transformation(iris, t)
apply_transformation <- function(data, thresholds) {
  d = cbind(data)
  for (i in 1:nrow(data)) {
    for (k in 1:length(thresholds)) {
        t = thresholds[[k]]
        j = t[[1]]
        th = t[[2]]
        d[i, j] = as.integer(length(th) + 1);
        if (!is.null(th)) { # it th is null then we have only 1 slice
            for (l in 1:length(th)) {
              if (data[i, j] < th[l]) {
                d[i, j] = as.integer(l) # we assume that discrete values are based on thresholds indices
                break;
              }
            }
          }
      }
    }
  return(d)
}
