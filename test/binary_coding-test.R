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

sum((test$Class - predict(l1, test))^2)
sum((test2$Class - predict(l2, test2))^2)

l1
l2

