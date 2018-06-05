# testing
iris_c = iris
fil = seq(from=1, to=150, by=2)
iris_c[fil, 1] = NA
iris_c[1:10,]
p = standarization.prepare(iris_c, c(1))
p[[1]]
after1 = standarization.apply(iris_c, p)
after1[1:10,]

library(e1071)
data("Sonar")

err <- function(y.true, y.pred) { sum(y.pred!=y.true)/length(y.true) }

mean(Sonar[,1])
sd(Sonar[,1])

s <- standarization.prepare(Sonar, c(1))
s2 <- standarization.apply(Sonar, s)

mean(s2[,1])
sd(s2[,1])

v <- runif(nrow(Sonar))
train <- Sonar[v>=0.2,]
test <- Sonar[v<0.2,]

s <- svm(Class ~ ., train, kernel="linear")

err(test$Class, predict(s, test))

s <- standarization.prepare(train, seq(1,60))
train <- standarization.apply(train, s)
test <- standarization.apply(test, s)

s <- svm(Class ~ ., train, kernel="linear")

err(test$Class, predict(s, test))

data("Satellite")

summary(Satellite)

v <- runif(nrow(Satellite))
train <- Satellite[v>=0.2,]
test <- Satellite[v<0.2,]

s <- svm(classes ~ ., train, kernel="linear")

err(test$classes, predict(s, test))

s <- standarization.prepare(train, seq(1,36))
train <- standarization.apply(train, s)
test <- standarization.apply(test, s)

s <- svm(classes ~ ., train, kernel="linear")

err(test$classes, predict(s, test))

