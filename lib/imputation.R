#
# prepare_tranformation
#
prepare_transformation <- function(data, columnIndices, columnOptions) {
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
       cat("Wrong option: ", option, ", for column index ", col_i, sep="")
     }
     transformation_parameters[[i]] = list(col_i, val)
  }
  
  return(transformation_parameters)
}


#
# apply_tranformation
#
apply_transformation <- function(data, transformation_parameters) {
  data_copy = data
  for (i in 1:length(transformation_parameters)) {
    col_i = transformation_parameters[[i]][[1]]
    val = transformation_parameters[[i]][[2]]
    data_copy[which(is.na(data[, col_i])), col_i] = val
  }
  
  return(data_copy)
}

# testing
# I will put NA values in iris, NULL doesn't work here
iris_c = iris
fil = seq(from=1, to=150, by=2)
iris_c[fil, 1:2] = NA
p = prepare_transformation(iris_c, c(1, 2),  c("average", "median"))
after1 = apply_transformation(iris_c, p)
after1[1:10,]

letters = matrix(, nrow=5, ncol=1)
letters[1,] = "A"
letters[2,] = "A"
letters[3,] = "B"
letters[4,] = "C"
p2 = prepare_transformation(letters, c(1), c("mode"))
after2 = apply_transformation(letters, p2)
after2
