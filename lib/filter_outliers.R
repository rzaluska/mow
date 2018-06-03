#' filter_outliners.prepare - compute first and third quadrile values per column
#'
#' @param data 
#' @param columnIndices - columns selected for transformation
#'
#' @return first and third quadrile values per column
#' @export
#'
#' @examples
#' q <- filter_outliners.prepare(iris, columnIndices = c(1,2,3,4))
filter_outliners.prepare <- function(data, columnIndices) {
  cols_first_q <- list()
  cols_third_q <- list()
  
  used_cols_ids <- list()
  
  k <- 1
  for (i in names(data)) {
    col_id = which(colnames(data)==i)
    if (missing(columnIndices)) {
      used_cols_ids[k] <- col_id
      k = k + 1
    }
    else if (col_id %in% columnIndices) {
        used_cols_ids[k] <- col_id
        k = k + 1
      }
  }
  
  for (i in used_cols_ids) {
    col <- data[,i]
    q1 <- quantile(col, 0.25)[[1]]
    q3 <- quantile(col, 0.75)[[1]]
    
    cols_first_q[i] <- q1
    cols_third_q[i] <- q3
  }
  
  return(list(cols_first_q, cols_third_q))
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


#' filter_outliners.apply - removes rows where one of values in outliners columns meets filtering criteria
#'
#' @param data 
#' @param beta - smaller beta - more agresive filtering
#' @param quadrils 
#'
#' @return
#' @export
#'
#' @examples
#' q <- filter_outliners.prepare(iris, columnIndices = c(1,2,3,4))
#' l <- filter_outliners.apply(iris, 0.1, q)
#' irisf <- iris[l,]
filter_outliners.apply <- function(data, beta, quadrils) {
  logical_mask <- rep(T, nrow(data))
  q1 <- quadrils[[1]]
  q3 <- quadrils[[2]]
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      if (index_exits(j, q1) && index_exits(j, q3)) {
        if (data[i,j] > q3[[j]] + beta * (q3[[j]] - q1[[j]])) {
          # too big value
          logical_mask[i] <- logical_mask[i] & FALSE
        }
        if (data[i,j] < q1[[j]] - beta * (q3[[j]] - q1[[j]])) {
          logical_mask[i] <- logical_mask[i] & FALSE
        }
      }
    }
  }
  return(logical_mask)
}
