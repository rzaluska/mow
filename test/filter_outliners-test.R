library(e1071)

v <- runif(nrow(iris))
train <- iris[v>=0.2,]
test <- iris[v<0.2,]

err <- function(y.true, y.pred) { sum(y.pred!=y.true)/length(y.true) }

b <- naiveBayes(Species ~., train)
cat(err(test$Species, predict(b, test[,-1])), "\n")

q <- filter_outliners.prepare(train, c(1,2,3,4))
train <- train[filter_outliners.apply(train, 0.5, q),]

b <- naiveBayes(Species ~., train)
cat(err(test$Species, predict(b, test[,-1])), "\n")

test <- test[filter_outliners.apply(test, 0.5, q),]
cat(err(test$Species, predict(b, test[,-1])), "\n")
