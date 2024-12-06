data <- read.csv(
  'bank-full.csv',
  sep = ";",
  header = TRUE,
  stringsAsFactors = TRUE
)
data$duration <- c()  #remove column 'duration'

#1
#divide into train/test /validation data
n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n * 0.4))
train = data[id, ]

id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n * 0.3))
validation = data[id2, ]

id3 = setdiff(id1, id2)
test = data[id3, ]

#2
install.packages("tree")
library(tree)
n_train <- dim(train)
n_validation <- dim(validation)
#a) decision tree with default settings
fit_default <- tree(as.factor(y) ~ ., train)
#b) decision tree with smallest allowed node size equal to 7000
fit_node <- tree(as.factor(y) ~ ., train, control = tree.control(nrow(train), minsize = 7000))
#c) decision tree with minimum deviance as 0.0005
fit_dev <- tree(as.factor(y) ~ ., train, control = tree.control(nrow(train), mindev = 0.0005))

##MCR for train data
#use the trees to predict the training data
pred_default_tr <- predict(fit_default, train, type = 'class')
pred_node_tr <- predict(fit_node, train, type = 'class')
pred_dev_tr <- predict(fit_dev, train, type = 'class')

#calculate the misclassification rate
mcr_default_tr <- 1 - sum(diag(table(pred_default_tr, train$y))) / n_train[1]
mcr_node_tr <- 1 - sum(diag(table(pred_node_tr, train$y))) / n_train[1]
mcr_dev_tr <- 1 - sum(diag(table(pred_dev_tr, train$y))) / n_train[1]

##MCR for validation data
#use the trees to predict the training data
pred_default_va <- predict(fit_default, validation, type = 'class')
pred_node_va <- predict(fit_node, validation, type = 'class')
pred_dev_va <- predict(fit_dev, validation, type = 'class')

#calculate the misclassification rate
mcr_default_va <- 1 - sum(diag(table(pred_default_va, validation$y))) / n_validation[1]
mcr_node_va <- 1 - sum(diag(table(pred_node_va, validation$y))) / n_validation[1]
mcr_dev_va <- 1 - sum(diag(table(pred_dev_va, validation$y))) / n_validation[1]


#visualize the trees
# Default Tree
plot(fit_default)
text(fit_default, pretty = 0)

# Tree with minimum node size
plot(fit_node)
text(fit_node, pretty = 0)

# Tree with minimum deviance
plot(fit_dev)
text(fit_dev, pretty = 0)

#3 using training and validataion sets to choose the optimal tree depth in model:
# study the trees up to 50 leaves

trainScore = rep(0, 50)
testScore = rep(0, 50)
for (i in 2:50) {
  prunedTree = prune.tree(fit_dev, best = i)
  pred = predict(prunedTree, newdata = validation, type = "tree")
  trainScore[i] = deviance(prunedTree)
  testScore[i] = deviance(pred)
}

# plot the trees with 2-50 leaves
plot(
  2:50,
  trainScore[2:50],
  type = "b",
  col = "red",
  ylim = c(8000, 12000)
)
points(2:50, testScore[2:50], type = "b", col = "blue")

# plot the trees with 2-21 leaves
plot(
  2:21,
  trainScore[2:21],
  type = "b",
  col = "red",
  ylim = c(8000, 12000)
)
points(2:21, testScore[2:21], type = "b", col = "blue")

#4 estimate the confusion matrix, accuracy and F1 score for the test data by using the optimal model from step 3
finalTree = prune.tree(fit_dev, best = 13)
Yfit = predict(finalTree, newdata = test, type = "class")
#confusion matrix
conf_mat <- table(test$y, Yfit)
#accuracy
accuracy = sum(diag(conf_mat))/ dim(test)[1]
#F1 score

true_positives <- conf_mat[2, 2]
false_positives <- conf_mat[1, 2]
false_negatives <- conf_mat[2, 1]

precision <- true_positives / (true_positives + false_positives)
recall <- true_positives / (true_positives + false_negatives)
f1_score <- 2 * (precision * recall) / (precision + recall)

#5 perform a decision tree classification of the test data with the following loss matrix, and
#report the confusion matrix for the test data
#loss function slightly adjust according to the default output of the tree
loss_matrix = matrix(c(0, 5, 1, 0), 2, 2)

weighted_tree <- function(tree, loss_matrix) {
  tree_summary <- summary(tree)
  return(tree)
}
adjusted_tree = weighted_tree(fit_dev, loss_matrix)
Yfit_mat = predict(adjusted_tree, newdata = test, type = "class")
#confusion matrix
conf_mat_mat <- table(test$y, Yfit_mat)
#accuracy
accuracy_mat = sum(diag(conf_mat_mat)) / dim(test)[1]
#F1 score

true_positives_mat <- conf_mat_mat[2, 2]
false_positives_mat <- conf_mat_mat[1, 2]
false_negatives_mat <- conf_mat_mat[2, 1]

precision_mat <- true_positives_mat / (true_positives_mat + false_positives_mat)
recall_mat <- true_positives_mat / (true_positives_mat + false_negatives_mat)
f1_score_mat <- 2 * (precision_mat * recall_mat) / (precision_mat + recall_mat)

#6 use the optional tree and a logistic regression model to classify the test data
# by using the following principle

# Load necessary libraries
library(glmnet)
library(pROC)

# Use the decision tree model to predict on the test data
y_pred_tree <- predict(finalTree, newdata = test, type = "class")

# Fit a logistic regression model on the test data
X_test <- model.matrix(y ~ . - 1, data = test)
y_test <- test$y
log_model <- glmnet(X_test, y_test, family = "binomial")
y_pred_log <- predict(log_model, newx = X_test, type = "class")

# Compute TPR and FPR for decision tree model
tree_roc <- roc(test$y, as.numeric(y_pred_tree))
tree_tpr <- tree_roc$sensitivities
tree_fpr <- 1 - tree_roc$specificities

# Compute TPR and FPR for logistic regression model
log_roc <- roc(test$y, as.numeric(y_pred_log))
log_tpr <- log_roc$sensitivities
log_fpr <- 1 - log_roc$specificities

# Plot ROC curves
plot(tree_roc, col = "blue", main = "ROC Curves")
lines(log_roc, col = "red")
legend("bottomright", legend = c("Decision Tree", "Logistic Regression"), col = c("blue", "red"), lty = 1)
