
install.packages("randomForest")
library(randomForest)

#build test datatest
set.seed(1234)
x1 <- runif(1000)
x2 <- runif(1000)
testdata <- cbind(x1, x2)
y <- as.numeric(x1 < x2)
testlabels <- as.factor(y)

#build the train dataset for 1000 times with the size of 100

error_rate_1<- list(
  number1 = rep(0,1000),
  number2 = rep(0,1000),
  number3 = rep(0,1000)
)
mean_error_1<- c()
var_error_1<- c()
for (i in 1:1000) {
  x1 <- runif(100)
  x2 <- runif(100)
  trdata <- cbind(x1, x2)
  y <- as.numeric(x1 < x2)
  trlabels <- as.factor(y)

  #build the models
  rf_model1 <- randomForest(trdata, trlabels, ntree = 1, nodesize = 25, keep.forest = TRUE)
  rf_model2 <- randomForest(trdata, trlabels, ntree = 10, nodesize = 25, keep.forest = TRUE)
  rf_model3 <- randomForest(trdata, trlabels, ntree = 100, nodesize = 25, keep.forest = TRUE)

  #predictions and error rates
  predictions1<- predict(rf_model1,testdata)
  error_rate_1$number1[i] <- mean(predictions1 != testlabels)
  predictions2<- predict(rf_model2,testdata)
  error_rate_1$number2[i] <- mean(predictions2 != testlabels)
  predictions3<- predict(rf_model3,testdata)
  error_rate_1$number3[i] <- mean(predictions3 != testlabels)

}

#compute the mean and variance of error rates
mean_error_1[1]<- mean(error_rate_1$number1)
mean_error_1[2]<- mean(error_rate_1$number2)
mean_error_1[3]<- mean(error_rate_1$number3)

var_error_1[1] <- var(error_rate_1$number1)
var_error_1[2] <- var(error_rate_1$number2)
var_error_1[3] <- var(error_rate_1$number3)



#change the conditions of x1/x3
#build test datatest
set.seed(1234)
x3 <- runif(1000)
x4 <- runif(1000)
testdata2 <- cbind(x3, x4)
y2 <- as.numeric(x3 < 0.5)
testlabels2 <- as.factor(y2)

#build the train dataset for 1000 times with the size of 100

error_rate_2<- list(
  number1 = rep(0,1000),
  number2 = rep(0,1000),
  number3 = rep(0,1000)
)
mean_error_2<- c()
var_error_2<- c()
for (i in 1:1000) {
  x3 <- runif(100)
  x4 <- runif(100)
  trdata <- cbind(x3, x4)
  y <- as.numeric(x3 < 0.5)
  trlabels <- as.factor(y)

  #build the models
  rf_model1 <- randomForest(trdata, trlabels, ntree = 1, nodesize = 25, keep.forest = TRUE)
  rf_model2 <- randomForest(trdata, trlabels, ntree = 10, nodesize = 25, keep.forest = TRUE)
  rf_model3 <- randomForest(trdata, trlabels, ntree = 100, nodesize = 25, keep.forest = TRUE)

  #predictions and error rates
  predictions1<- predict(rf_model1,testdata2)
  error_rate_2$number1[i] <- mean(predictions1 != testlabels2)
  predictions2<- predict(rf_model2,testdata2)
  error_rate_2$number2[i] <- mean(predictions2 != testlabels2)
  predictions3<- predict(rf_model3,testdata2)
  error_rate_2$number3[i] <- mean(predictions3 != testlabels2)

}

#compute the mean and variance of error rates
mean_error_2[1]<- mean(error_rate_2$number1)
mean_error_2[2]<- mean(error_rate_2$number2)
mean_error_2[3]<- mean(error_rate_2$number3)

var_error_2[1] <- var(error_rate_2$number1)
var_error_2[2] <- var(error_rate_2$number2)
var_error_2[3] <- var(error_rate_2$number3)

#change the conditions of x1/x3 and nodesize
#build test datatest
set.seed(1234)
x5 <- runif(1000)
x6 <- runif(1000)
testdata3 <- cbind(x5, x6)
y3 <- as.numeric((x5<0.5 & x6<0.5) | (x5>0.5 & x6>0.5))
testlabels3 <- as.factor(y3)

#build the train dataset for 1000 times with the size of 100

error_rate_3<- list(
  number1 = rep(0,1000),
  number2 = rep(0,1000),
  number3 = rep(0,1000)
)
mean_error_3<- c()
var_error_3<- c()
for (i in 1:1000) {
  x5 <- runif(100)
  x6 <- runif(100)
  trdata <- cbind(x5, x6)
  y <- as.numeric((x5<0.5 & x6<0.5) | (x5>0.5 & x6>0.5))
  trlabels <- as.factor(y)

  #build the models
  rf_model1 <- randomForest(trdata, trlabels, ntree = 1, nodesize = 12, keep.forest = TRUE)
  rf_model2 <- randomForest(trdata, trlabels, ntree = 10, nodesize = 12, keep.forest = TRUE)
  rf_model3 <- randomForest(trdata, trlabels, ntree = 100, nodesize = 12, keep.forest = TRUE)

  #predictions and error rates
  predictions1<- predict(rf_model1,testdata3)
  error_rate_3$number1[i] <- mean(predictions1 != testlabels3)
  predictions2<- predict(rf_model2,testdata3)
  error_rate_3$number2[i] <- mean(predictions2 != testlabels3)
  predictions3<- predict(rf_model3,testdata3)
  error_rate_3$number3[i] <- mean(predictions3 != testlabels3)

}

#compute the mean and variance of error rates
mean_error_3[1]<- mean(error_rate_3$number1)
mean_error_3[2]<- mean(error_rate_3$number2)
mean_error_3[3]<- mean(error_rate_3$number3)

var_error_3[1] <- var(error_rate_3$number1)
var_error_3[2] <- var(error_rate_3$number2)
var_error_3[3] <- var(error_rate_3$number3)

result<- list(
  mean_error_1 = mean_error_1,
  mean_error_2 = mean_error_2,
  mean_error_3 = mean_error_3,

  var_error_1 = var_error_1,
  var_error_2 = var_error_2,
  var_error_3 = var_error_3
)
print(result)

#' As the number of trees in the random forest increases, the mean error rate decreases. This happens because:
#' A random forest combines multiple decision trees. As more trees are added, the predictions are averaged, reducing the variance of the model.
#' With more trees, the random forest effectively samples a larger variety of subsets from the training data. This increases the likelihood that the model captures the true underlying patterns
#' Random forests are capable of capturing non-linear patterns effectively by aggregating multiple decision trees.
#' Despite being more complex, the random forest performs better on the third dataset with sufficient trees because:
#' With enough trees, the random forest can approximate complex decision boundaries like the one in the third dataset.
#' The increased complexity of the third dataset provides a stronger signal for the model to learn from. This reduces the risk of the random forest overfitting to noise, especially when the dataset is large and diverse.

set.seed(1234567890)
max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log lik between two consecutive iterations
n=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow=n, ncol=D) # training data
true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")
# Producing the training data
for(i in 1:n) {
  m <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[i,d] <- rbinom(1,1,true_mu[m,d])
  }
}
M=3 # number of clusters
w <- matrix(nrow=n, ncol=M) # weights
pi <- vector(length = M) # mixing coefficients
mu <- matrix(nrow=M, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations
# Random initialization of the parameters
pi <- runif(M,0.49,0.51)
pi <- pi / sum(pi)
for(m in 1:M) {
  mu[m,] <- runif(D,0.49,0.51)
}
pi
mu
for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  Sys.sleep(0.5)
  # E-step: Computation of the weights
  # Your code here
  for (i in 1:n) {
    for (m in 1:M) {
      numerator <- pi[m] * prod(mu[m, ]^x[i, ] * (1 - mu[m, ])^(1 - x[i, ]))
      denominator <- sum(sapply(1:M, function(k) {
        pi[k] * prod(mu[k, ]^x[i, ] * (1 - mu[k, ])^(1 - x[i, ]))
      }))
      w[i, m] <- numerator / denominator
    }
  }

  #M-step: ML parameter estimation from the data and weights
  # Your code here

  for (m in 1:M) {
    pi[m] <- sum(w[, m]) / n
    for (d in 1:D) {
      mu[m, d] <- sum(w[, m] * x[, d]) / sum(w[, m])
    }
  }
  #Log likelihood computation.
  # Your code here

  llik[it] <- sum(sapply(1:n, function(i) {
    log(sum(sapply(1:M, function(m) {
      pi[m] * prod(mu[m, ]^x[i, ] * (1 - mu[m, ])^(1 - x[i, ]))
    })))
  }))

  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  flush.console()
  # Stop if the lok likelihood has not changed significantly
  # Your code here
  if (it > 1 && abs(llik[it] - llik[it - 1]) < min_change) {
    cat("Converged at iteration", it, "\n")
    break
  }
}
pi
mu
plot(llik[1:it], type="o")
