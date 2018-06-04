library(e1071)
data("Sonar")

err <- function(y.true, y.pred) { sum(y.pred!=y.true)/length(y.true) }

summary(Sonar)

mean(Sonar[,1])
sd(Sonar[,1])

s <- standarization.prepare(Sonar, c(1))
s2 <- standarization.apply(Sonar, s)

mean(s2[,1])
sd(s2[,1])

v <- runif(nrow(Sonar))
train <- Sonar[v>=0.2,]
test <- Sonar[v<0.2,]

svm <- svm(Class ~ ., train)

err(test$Class, predict(svm, test))

s <- standarization.prepare(train, seq(1,60))
train <- standarization.apply(train, s)
test <- standarization.apply(test, s)

svm <- svm(Class ~ ., train)

err(test$Class, predict(svm, test))

data("Satellite")

summary(Satellite)

v <- runif(nrow(Satellite))
train <- Satellite[v>=0.2,]
test <- Satellite[v<0.2,]

svm <- svm(classes ~ ., train)

err(test$classes, predict(svm, test))

s <- standarization.prepare(train, seq(1,36))
train <- standarization.apply(train, s)
test <- standarization.apply(test, s)

svm <- svm(classes ~ ., train)

err(test$classes, predict(svm, test))

