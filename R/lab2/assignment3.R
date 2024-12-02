#task1
setwd("D:/lab2")
install.packages("tidyverse")
library(tidyverse)
install.packages("caret")
library(caret)

data <- read.csv("communities.csv")

data_features <- data %>% select(-ViolentCrimesPerPop)

scale_parameters<- preProcess(data_features)
data_scaled <- predict(scale_parameters,data_features)

cov_matrix <- cov(data_scaled)

eigen_results <- eigen(cov_matrix)

eigen_values <- eigen_results$values

var_explained <- cumsum(eigen_values) / sum(eigen_values)

num_components <- which(var_explained >= 0.95)[1]
cat("the number of components are needed to obtain at
least 95% of variance:", num_components, "\n")


pc1_pro <- eigen_values[1] / sum(eigen_values)
pc2_pro <- eigen_values[2] / sum(eigen_values)
cat("The proportion of variance explained by PC1:", pc1_pro, "\n")
cat("The proportion of variance explained by PC2:", pc2_pro, "\n")

#task2

pca_results <- princomp(data_scaled)

loadings <- pca_results$loadings

barplot(abs(loadings[, 1]), main = "Trace Plot of PC1", las = 2, col = "steelblue")

top5_features <- sort(abs(loadings[, 1]), decreasing = TRUE)[1:5]
cat("The top 5 contributing features in PC1:\n")
print(names(top5_features))

scores <- as.data.frame(pca_results$scores)
scores$ViolentCrimesPerPop <- data$ViolentCrimesPerPop

library(ggplot2)
ggplot(scores, aes(x = Comp.1, y = Comp.2, color = ViolentCrimesPerPop)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "PC1 vs PC2 with ViolentCrimesPerPop", x = "PC1", y = "PC2") +
  theme_minimal()

#task3

set.seed(12345)
train_indices <- sample(1:nrow(data), size = nrow(data) / 2)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

scale_para<- preProcess(train_data)
train_scaled<- predict(scale_para,train_data)
test_scaled<- predict(scale_para,test_data)

model <- lm(ViolentCrimesPerPop ~ ., data = train_scaled)

train_pred <- predict(model, train_scaled)
train_mse <- mean((train_scaled$ViolentCrimesPerPop - train_pred)^2)
cat("training (MSE)：", train_mse, "\n")


test_pred <- predict(model, test_scaled)
test_mse <- mean((test_scaled$ViolentCrimesPerPop - test_pred)^2)
cat("test (MSE)：", test_mse, "\n")

#task4

cost_function <- function(theta, X, y) {
  predictions <- X %*% theta
  mse <- mean((y - predictions)^2)
  return(mse)
}

X_train <- as.matrix(train_scaled %>% select(-ViolentCrimesPerPop))
y_train <- as.matrix(train_scaled$ViolentCrimesPerPop)
X_test <- as.matrix(test_scaled %>% select(-ViolentCrimesPerPop))
y_test <- as.matrix(test_scaled$ViolentCrimesPerPop)

theta_init <- rep(0, ncol(X_train))

train_errors <- c()
test_errors <- c()

cost_function_with_logging <- function(theta) {
  train_error <- cost_function(theta, X_train, y_train)
  test_error <- cost_function(theta, X_test, y_test)

  # Log errors
  train_errors <<- c(train_errors, train_error)
  test_errors <<- c(test_errors, test_error)

  return(train_error)  # Return train error for optimization
}

optim_results <- optim(
  par = theta_init,
  fn = cost_function_with_logging,
  method = "BFGS"
)

# Plot errors, discard initial iterations for clarity
plot(501:length(train_errors), train_errors[501:length(train_errors)], type = "l", col = "blue",
     xlab = "Iteration", ylab = "Error", main = "Error vs Iteration")
lines(501:length(test_errors), test_errors[501:length(test_errors)], col = "red")
legend("topright", legend = c("Training Error", "Test Error"), col = c("blue", "red"), lty = 1)


# Find optimal iteration
optimal_iteration <- which.min(test_errors)
cat("Optimal iteration:", optimal_iteration, "\n")
cat("Training MSE at optimal iteration:", train_errors[optimal_iteration], "\n")
cat("Test MSE at optimal iteration:", test_errors[optimal_iteration], "\n")
