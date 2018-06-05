# testing
letters = iris[1:5, 1:2]
letters[1,1] = "A"
letters[2,1] = "A"
letters[3,1] = "B"
letters[4,1] = "C"
letters[5,1] = "D"
p = binary_coding.prepare(letters, c(1))
after = binary_coding.apply(letters, p)
after

library(mlbench)
library(e1071)
data("Servo")

summary(Servo)
nrow(Servo)

p <- binary_coding.prepare(Servo, seq(4))
colnames(binary_coding.apply(Servo, p, F))
colnames(binary_coding.apply(Servo, p, T))

v <- runif(nrow(Servo))
train <- Servo[v>=0.2,]
test <- Servo[v<0.2,]
p <- binary_coding.prepare(Servo, seq(4))
train2 <- binary_coding.apply(Servo, p, T)[v>=0.2,]
test2 <- binary_coding.apply(Servo, p, T)[v<0.2,]
l1 <- lm(train$Class ~., train)
l2 <- lm(train2$Class ~., train2)

sum((test$Class - predict(l1, test))^2)/nrow(test)
sum((test2$Class - predict(l2, test2))^2)/nrow(test)

l1
l2

