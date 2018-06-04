library(mlbench)
data(Glass)

err <- function(y.true, y.pred) { sum(y.pred!=y.true)/length(y.true) }

summary(Glass)

type = 'even'
size = 5

d <- discretization.prepare(Glass, seq(9),
                            list(list(size, type),list(size, type),list(size, type), list(size, type),list(size, type),
                                 list(size, type),list(size, type),list(size, type),list(size, type)))
summary(discretization.apply(Glass, d))

v <- runif(nrow(Glass))
train <- Glass[v>=0.2,]
test <- Glass[v<0.2,]
tree1 <- rpart(Type ~., train)
err(test$Type, predict(tree1, test, type="class"))
b <- naiveBayes(Type ~., train)
err(test$Type, predict(b, test, type="class"))

type = 'even'
size = 10

d <- discretization.prepare(train,seq(9),
                       list(list(size, type),list(size, type),
                       list(size, type), list(size, type),list(size, type),
                       list(size, type),list(size, type),list(size, type),list(size, type)))

train2 <- discretization.apply(train, d)
test2 <- discretization.apply(test, d)
tree1 <- rpart(Type ~., train2)
err(test$Type, predict(tree1, test2, type="class"))
b <- naiveBayes(Type ~., train2)
err(test$Type, predict(b, test2, type="class"))
