# 
#' compute discreatization thresholds - prepare_transformation
#'
#' @param dataColumn -  column for which compute discreatization thresholds
#' @param numSlices - number of discrete values to produce
#' @param type - type of discreatization, one of:
#' - 'even' - split domain by even slices, where slice size is (max - min)/numSlices
#' - 'size' - split domain by slices of even numer of examples in them
#'
#' @return list of threshold values of size (numSlices - 1)
#' @export
#'
#' @examples
#' t = prepare_transformation(iris[,1], 5, 'even')
#' t = prepare_transformation(iris[,1], 5, 'size')
prepare_transformation <- function(dataColumn, numSlices, type) {
  if (! type %in% list('even', 'size')) {
    return(NULL)
  }
  tresholds = c()
  number_of_examples = length(dataColumn)
  if (type == 'even') {
    # we assume that data domain is fully enclosed between max and min values
    slice_size = (max(dataColumn) - min(dataColumn)) / numSlices
    current_index = 0
    currTreshold = min(dataColumn) + slice_size
    
    for (i in 1:(numSlices-1)) {
        tresholds[i] = currTreshold
        currTreshold = currTreshold + slice_size
      }
  }
  
  else if (type == 'size') {
    slice_size = as.integer(length(dataColumn) / numSlices)
    sorted = sort(dataColumn)
    current_index = 0
    curr_slice_size = slice_size
    
    for (i in 1:length(dataColumn)) {
      if (curr_slice_size == 0) {
        tresholds = c(tresholds, sorted[i])
        curr_slice_size = slice_size
      }
      else {
        curr_slice_size = curr_slice_size - 1
      }
    }
  }
  return(tresholds)
}


index_exits <- function(index, data) {
  if (index > length(data)) {
    return(FALSE)
  }
  
  if (is.null(data[[index]])) {
    return(FALSE)
  }
  return(TRUE)
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
#'  t = prepare_transformation(iris[,1], 5, 'even')
#'  transformed = apply_transformation(iris[,1],t)
apply_transformation <- function(dataColumn, thresholds) {
  d = c()
  for (i in 1:length(dataColumn)) {
    d[i] = as.integer(length(thresholds) + 1);
    for (j in 1:length(thresholds)) {
      if (dataColumn[i] < thresholds[j]) {
        d[i] = as.integer(j) # we assume that discrete values are based on thresholds indices
        break;
      }
    }
  }
  return(d)
}
