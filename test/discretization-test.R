library(mlbench)
data(Glass)

err <- function(y.true, y.pred) { sum(y.pred!=y.true)/length(y.true) }

summary(Glass)

v <- runif(nrow(Glass))
train <- Glass[v>=0.2,]
test <- Glass[v<0.2,]
tree1 <- rpart(Type ~., train)
err(test$Type, predict(tree1, test, type="class"))
b <- naiveBayes(Type ~., train)
err(test$Type, predict(b, test, type="class"))

type = 'size'
size = 10

d <- discretization.prepare(train, c(1,2,3,4,5,6,7,8),
                       list(list(size, type),list(size, type), list(size, type),list(size, type),
                       list(size, type),list(size, type),list(size, type),list(size, type)))

train <- discretization.apply(train, d)
test <- discretization.apply(test, d)
tree1 <- rpart(Type ~., train)
err(test$Type, predict(tree1, test, type="class"))
b <- naiveBayes(Type ~., train)
err(test$Type, predict(b, test, type="class"))
