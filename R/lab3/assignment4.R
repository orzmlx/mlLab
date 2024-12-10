#assignment4
#task1
install.packages("neuralnet")
library(neuralnet)
set.seed(1234567890)
Var <- runif(500, 0, 10)
mydata <- data.frame(Var, Sin=sin(Var))
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test

# Random initialization of the weights in the interval [-1, 1]
n_input <- 1
n_hidden <- 10
n_output <- 1
n_weights_input_to_hidden <- n_input * n_hidden
n_bias_hidden <- n_hidden
n_weights_hidden_to_output <- n_hidden * n_output
n_bias_output <- n_output
total_weights <- n_weights_input_to_hidden + n_bias_hidden + n_weights_hidden_to_output + n_bias_output

set.seed(1234567890)
winit <- runif(total_weights, -1, 1)
nn <- neuralnet(Sin ~ Var, data=tr, hidden = 10, act.fct="logistic",
                startweights = winit, linear.output = FALSE)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn,te), col="red", cex=1)

#task2

act_linear <- function(x) { x }
act_relu <- function(x) { ifelse(x > 0, x, 0) }
act_softplus <- function(x) { log(1 + exp(x)) }

nn_linear <- neuralnet(Sin ~ Var, data=tr, hidden = 10, act.fct=act_linear, startweights = winit, linear.output=TRUE)
nn_relu <- neuralnet(Sin ~ Var, data=tr, hidden=10, act.fct=act_relu, startweights = winit, linear.output=FALSE)
nn_softplus <- neuralnet(Sin ~ Var, data=tr, hidden=10, act.fct=act_softplus, startweights = winit, linear.output=FALSE)


plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1], predict(nn_linear, te), col="red", cex=1)
points(te[,1], predict(nn_relu, te), col="green", cex=1)
points(te[,1], predict(nn_softplus, te), col="yellow", cex=1)

#Task3

new_points <- data.frame(Var = seq(0, 50, length.out = 500))
predictions_new <- predict(nn, new_points)

plot(new_points$Var, sin(new_points$Var), col="blue", type='l', lwd=2)
lines(new_points$Var, predictions_new, col="red", lwd=2)

#Task5

nn_inverse <- neuralnet(Var ~ Sin, data=mydata, hidden=10, act.fct="logistic", linear.output=TRUE,threshold = 0.1)

plot(mydata$Sin, mydata$Var, col="blue", pch=20)
predictions_inverse <- as.data.frame(predict(nn_inverse, data.frame(mydata$Sin)))
predictions_inverse <- predictions_inverse[, 1]
points(mydata$Sin, predictions_inverse, col="red", pch=20)

