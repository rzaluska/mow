# testing
iris_c = iris
fil = seq(from=1, to=150, by=2)
iris_c[fil, 1:2] = NA
p = imputation.prepare(iris_c, c(1, 2),  c("average", "median"))
after1 = imputation.apply(iris_c, p)
after1[1:10,]

letters = matrix(NA, nrow=5, ncol=1)
letters[1,] = "A"
letters[2,] = "A"
letters[3,] = "B"
letters[4,] = "C"
p2 = imputation.prepare(letters, c(1), c("mode"))
imputation.apply(letters, p2, mark_artificial_values = T)

data("HouseVotes84") # moda z tego
summary(HouseVotes84)
nrow(HouseVotes84)

m <- imputation.prepare(HouseVotes84, seq(2,17), c("mode","mode","mode","mode","mode","mode","mode","mode","mode","mode","mode","mode","mode","mode","mode","mode"))
HV <- imputation.apply(HouseVotes84, m)
summary(HV)

data("Ozone")
summary(Ozone)
nrow(Ozone)
mean(Ozone$V9)
sd(Ozone$V9)

m1 <- imputation.prepare(Ozone, c(9), c("average"))
Ozone2 <- imputation.apply(Ozone, m1)
summary(Ozone2$V9)
mean(Ozone2$V9)
sd(Ozone2$V9)

m1 <- imputation.prepare(Ozone, c(9), c("median"))
Ozone3 <- imputation.apply(Ozone, m1)
summary(Ozone3$V9)
mean(Ozone3$V9)
sd(Ozone3$V9)

v <- runif(nrow(HouseVotes84))
train <- HouseVotes84[v>=0.2,]
test <- HouseVotes84[v<0.2,]
tree1 <- rpart(Class ~., train)
err(test$Class, predict(tree1, test[,-1,], type="class"))
b <- naiveBayes(Class ~., train)
err(test$Class, predict(b, test[,-1,], type="class"))

m <- imputation.prepare(train, seq(2,17), c("mode","mode","mode","mode","mode","mode","mode","mode","mode","mode","mode","mode","mode","mode","mode","mode"))
train <- imputation.apply(train, m)
tree2 <- rpart(Class ~., train)
err(test$Class, predict(tree2, test[,-1,], type="class"))
b <- naiveBayes(Class ~., train)
err(test$Class, predict(b, test[,-1,], type="class"))
