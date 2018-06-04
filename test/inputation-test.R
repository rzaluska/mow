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
