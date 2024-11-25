setwd("D:/lab1")
#Divide the data into training and test data (60/40) and scale it appropriately.
data <- read.csv("parkinsons.csv")
set.seed(12345)
ini_sample<- sample(1:nrow(data),0.6*nrow(data))
train_data<- data[ini_sample,]
test_data<- data[-ini_sample,]

# Scale the dataset appropriately.
install.packages("caret")
library(caret)
scale_para<- preProcess(train_data)
train_data_scaled<- predict(scale_para,train_data)
test_data_scaled<- predict(scale_para,test_data)
#sacale_data<- train_data[,names(train_data)!="motor_UPDRS"]
#scale_para<- preProcess(sacale_data)
#train_data_scaled<- predict(scale_para,train_data)
#test_data_scaled<- predict(scale_para,test_data)
#train_data_scaled$motor_UPDRS <- train_data$motor_UPDRS
#test_data_scaled$motor_UPDRS <- test_data$motor_UPDRS

#Compute a linear regression model , estimate training and test MSE
model<- lm(motor_UPDRS ~ .-subject. -age -sex -test_time -total_UPDRS - 1,train_data_scaled)
train_prediction<- predict(model,train_data_scaled)
train_mse<- mean((train_prediction - train_data_scaled$motor_UPDRS)^2)
test_prediction<- predict(model,test_data_scaled)
test_mse<- mean((test_prediction - test_data_scaled$motor_UPDRS)^2)
summary(model)

# loglikelihood function that for a given parameter vector theta and dispersion sigma.
logLikelihood <- function(theta, sigma, x, y) {
  n <- length(y)
  predictions <- x %*% theta
  residuals <- y - predictions
  log_likelihood <- -0.5 * n * log(2 * pi * sigma^2) - (t(residuals) %*% residuals) / (2 * sigma^2)
  return(as.numeric(log_likelihood))
}

#Ridge function that for given vector theta, scalar sigma and scalar lambda and adds up a Ridge penalty to the minus loglikelihood.
ridge <- function(theta, sigma, lambda, x, y) {
  log_likelihood <- logLikelihood(theta, sigma, x, y)
  ridge_penalty <- lambda * sum(theta^2)
  return(-log_likelihood + ridge_penalty)
}

# Use function optim() with method=”BFGS” to find the optimal theta and sigma for the given lambda.
ridgeopt <- function(lambda, x, y) {
  n <- ncol(x)
  init_params <- c(rep(0, n), 1)
  ridge_obj <- function(params) {
    theta <- params[1:n]
    sigma <- params[n + 1]
    return(ridge(theta, sigma, lambda, x, y))
  }
  opt <- optim(init_params, ridge_obj, method = "BFGS")
  theta_opt <- opt$par[1:n]
  sigma_opt <- opt$par[n + 1]
  return(list(theta = theta_opt, sigma = sigma_opt))
}

#computes the degrees of freedom of the Ridge model based on the training data.
freedom_degree <- function(lambda, x) {
  xT <- t(x) %*% x
  heat <- x %*% solve(xT + lambda * diag(ncol(x))) %*% t(x)
  df <- sum(diag(heat)) #trace
  return(df)
}

#By using function RidgeOpt, compute optimal theta parameters for different lambda values.
train_data2<- as.matrix(train_data_scaled[,names(train_data)!="motor_UPDRS"])
test_data2<- as.matrix(test_data_scaled[,names(test_data)!="motor_UPDRS"])
train_value<- train_data_scaled$motor_UPDRS
test_value<- test_data_scaled$motor_UPDRS

lambda_values <- c(1, 100, 1000)

train_mse2<- c()
test_mse2<- c()
df<- c()
theta_value<- list()
for (i in seq_along(lambda_values)){
  lambda<- lambda_values[i]
  ridgemodel<- ridgeopt(lambda,train_data2,train_value)
  thetavalue<- ridgemodel$theta

  theta_value[[i]]<- thetavalue

  train_predictions<- train_data2 %*% thetavalue
  train_mse2[i]<- mean((train_value - train_predictions)^2)

  test_predictions<- test_data2 %*% thetavalue
  test_mse2[i]<- mean((test_value - test_predictions)^2)

  df[i]<- freedom_degree(lambda,train_data2)

  result<- list(
    train_mse2 = train_mse2,
    test_mse2 = test_mse2,
    df = df,
    theta_value = theta_value
  )
}
print(result)



