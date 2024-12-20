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
                startweights = winit, linear.output = TRUE)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn,te), col="red", cex=1)

#task2

act_linear <- function(x) { x }
act_relu <- function(x) { ifelse(x > 0, x, 0) }
act_softplus <- function(x) { log(1 + exp(x)) }

nn_linear <- neuralnet(Sin ~ Var, data=tr, hidden = 10, act.fct=act_linear, startweights = winit, linear.output=TRUE)
nn_relu <- neuralnet(Sin ~ Var, data=tr, hidden=10, act.fct=act_relu, startweights = winit, linear.output=TRUE)
nn_softplus <- neuralnet(Sin ~ Var, data=tr, hidden=10, act.fct=act_softplus, startweights = winit, linear.output=TRUE)


plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1], predict(nn_linear, te), col="red", cex=1)  
points(te[,1], predict(nn_relu, te), col="green", cex=1)  
points(te[,1], predict(nn_softplus, te), col="yellow", cex=1)  

#Task3

set.seed(1234567890)
new_Var <- runif(500, 0, 50)
new_mydata <- data.frame(new_Var, new_Sin = sin(new_Var))

new_tr <- new_mydata[1:25, ]
new_te <- new_mydata[26:500, ]

winit <- runif(10 * 2 + 10 + 1, -1, 1)
new_nn <- neuralnet(new_Sin ~ new_Var, data = new_tr, hidden = 10, startweights = winit, linear.output = TRUE)

plot(new_tr$new_Var,new_tr$new_Sin, xlab = "new_Var", ylab = "new_Sin", col = "black", cex = 2)
points(new_te, col = "blue", cex = 1)
points(new_te$new_Var, predict(new_nn, new_te), col = "red", cex = 1)

#task4
input_hidden_weight<- new_nn$weights[[1]][[1]]
print(input_hidden_weight)
hidden_output_weight<- new_nn$weights[[1]][[2]]
print(hidden_output_weight)

#Task5
new_points2 <- data.frame(Var = seq(0, 10, length.out = 500)) 
new_points2$Sin <- sin(new_points2$Var)

nn_inverse <- neuralnet(Var ~ Sin, data=new_points2, hidden=10, act.fct="logistic", linear.output=TRUE,threshold = 0.1)

plot(new_points2$Sin, new_points2$Var, col="blue", pch=20)  
predictions_inverse <- as.data.frame(predict(nn_inverse, data.frame(new_points2$Sin)))
predictions_inverse <- predictions_inverse[, 1]
points(new_points2$Sin, predictions_inverse, col="red", pch=20)





