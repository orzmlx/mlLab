#Assignment 2

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


library(rpart)
X_train <- model.matrix(y ~ . - 1, data = train)
y_train <- train$y
X_test <- model.matrix(y ~ . - 1, data = test)
y_test <- test$y
# Assuming the decision tree and logistic regression models are already trained
logreg_model <- glm(y ~ ., data = train_data, family = binomial)

# Train the optimal Decision Tree Model (assumed to be selected earlier)
finalTree <- rpart(y ~ ., data = train_data, method = "class", control = rpart.control(maxdepth = 13))
# Decision tree predictions (probabilities)
tree_pred_probs <- predict(finalTree, newdata = test, type = "prob")[,2]

# Logistic regression predictions (probabilities)
logreg_pred_probs <- predict(logreg_model, newdata = test, type = "response")
tree_pred_probs <- predict(finalTree, test, type = "prob")[, "yes"]
compute_tpr_fpr <- function(pred_probs, true_labels, threshold) {
  preds <- ifelse(pred_probs > threshold, "yes", "no")
  preds <- factor(preds, levels = c("no", "yes"))

  confusion <- table(Predicted = preds, Actual = true_labels)

  TP <- confusion[2, 2]  # Predicted = yes, Actual = yes
  FP <- confusion[2, 1]  # Predicted = yes, Actual = no
  FN <- confusion[1, 2]  # Predicted = no, Actual = yes
  TN <- confusion[1, 1]  # Predicted = no, Actual = no
  TPR <- TP / (TP + FN)  # True Positive Rate
  FPR <- FP / (FP + TN)  # False Positive Rate
  return(c(TPR, FPR))
}

# Calculate TPR and FPR for multiple thresholds
thresholds <- seq(0.05, 0.95, by = 0.05)
logistic_tpr_fpr <- sapply(thresholds, function(threshold) compute_tpr_fpr(logreg_pred_probs, test$y, threshold))
tree_tpr_fpr <- sapply(thresholds, function(threshold) compute_tpr_fpr(tree_pred_probs, test$y, threshold))

# Separate the TPR and FPR for logistic and decision tree
logistic_tpr <- logistic_tpr_fpr[1, ]
logistic_fpr <- logistic_tpr_fpr[2, ]
tree_tpr <- tree_tpr_fpr[1, ]
tree_fpr <- tree_tpr_fpr[2, ]
# Plot ROC curves for Logistic Regression and Decision Tree
plot(logistic_fpr, logistic_tpr, type = "l", col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate", lwd = 2)
lines(tree_fpr, tree_tpr, col = "red", lwd = 2)
legend("bottomright", legend = c("Logistic Regression", "Decision Tree"), col = c("blue", "red"), lwd = 2)
