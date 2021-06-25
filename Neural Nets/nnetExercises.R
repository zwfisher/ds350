set.seed(42)

X <- runif(200, -10, 10)
Y <- sin(X)

set.seed(42)
weight <- runif(10, -1, 1)

set.seed(42)
index <- sample(1:length(X), round(0.75*length(X)), replace = FALSE)
reg.train <- data.frame(X=X[index], Y=Y[index])
reg.test <- data.frame(X=X[-index], Y=Y[-index])

library(nnet)
set.seed(42)
reg.model.1 <- nnet(reg.train$X, reg.train$Y, size = 3, Wts = weight, maxit=50, linout=TRUE)
str(reg.model.1)

pred.model.1 <- predict(reg.model.1, data.frame(X=reg.test$X))
rmse <- sqrt(sum((reg.test$Y - pred.model.1)^2))

plot(sin, -10, 10)
points(reg.test$X, pred.model.1)

set.seed(42)
weight <- runif(22, -1, 1)
reg.model.2 <- nnet(reg.train$X, reg.train$Y, size = 7, Wts = weight, maxit=50, linout=TRUE)
str(reg.model.2)

pred.model.2 <- predict(reg.model.2, data.frame(X=reg.test$X))
rmse.2 <- sqrt(sum((reg.test$Y - pred.model.2)^2))

plot(sin, -10, 10)
points(reg.test$X, pred.model.2)

data <- iris

scale.data <- data.frame(lapply(data[,1:4], function(x) scale(x)))
scale.data$Species <- data$Species

index <- sample(1:nrow(scale.data), round(0.75*nrow(scale.data)), replace = FALSE)
clust.train <- scale.data[index,]
clust.test <- scale.data[-index,]

set.seed(42)
clust.model <- nnet(Species ~ ., data=clust.train, size=10, Wts=runif(83, -1, 1))

pred.model.clust <- predict(clust.model, clust.test[,1:4], type="class")
conf.mat <- table(clust.test$Species, pred.model.clust)
conf.mat

accuracy <- sum(diag(conf.mat))/sum(conf.mat)
accuracy
