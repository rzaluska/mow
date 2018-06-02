#
# prepare_tranformation - computes first and third quadrile for all columns or columns selected in columnIndices
#
prepare_transformation <- function(data, columnIndices) {
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
#
# apply_tranformation
# smaller beta - more agresive filtering
# to filter just apply
# q <- prepare_transformation(iris, columnIndices = c(1,2,3,4))
# l <- apply_transformation(iris, 0.1, q)
# irisf <- iris[l,]
#
apply_transformation <- function(data, beta, quadrils) {
  logica_mask <- rep(T, nrow(data))
  q1 <- quadrils[[1]]
  q3 <- quadrils[[2]]
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      if (index_exits(j, q1) && index_exits(j, q3)) {
        if (data[i,j] > q3[[j]] + beta * (q3[[j]] - q1[[j]])) {
          # too big value
          logica_mask[i] <- logica_mask[i] & FALSE
        }
        if (data[i,j] < q1[[j]] - beta * (q3[[j]] - q1[[j]])) {
          logica_mask[i] <- logica_mask[i] & FALSE
        }
      }
    }
  }
  return(logica_mask)
}