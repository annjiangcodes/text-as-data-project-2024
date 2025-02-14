# Load necessary libraries
library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textmodels)
library(caret)
library(readr)
library(purrr)
library(glmnet)

# Set working directory
setwd("/Users/azure/Desktop/Coursework/PhD Coursework/Text as Data/Homework/My Script/Measurement I Coding")
reddit <- read_csv("random_training_set_ann1+2+3+4.csv")

# Convert "Ann_Code" to numeric for glmnet; values: Positive (1), Neutral (0), Negative (-1)
reddit$Ann_Code <- as.numeric(reddit$Ann_Code)

# Create a corpus from the Reddit dataset
corpus_reddit <- corpus(reddit, text_field = "comment_body")
docvars(corpus_reddit, "Ann_Code") <- reddit$Ann_Code

# Preprocess text to create a document feature matrix without stemming
toks <- tokens(corpus_reddit, remove_punct = TRUE, remove_numbers = TRUE)
toks <- tokens_select(toks, stopwords("en"), selection = "remove")
dfm <- dfm(toks)

# Trim DFM to reduce sparsity by removing terms that appear in less than 0.5% of documents
dfm <- dfm_trim(dfm, min_docfreq = 0.005, docfreq_type = "prop")

# Split data into training and validation sets
set.seed(92073)  # Ensure reproducibility
docvars(corpus_reddit, "id_numeric") <- 1:ndoc(corpus_reddit)
alldocs <- 1:ndoc(corpus_reddit)
training <- sample(alldocs, round(length(alldocs) * 0.75))
validation <- alldocs[!alldocs %in% training]

# Create separate dfms for training and validation sets
dfmat_train <- dfm_subset(dfm, docvars(corpus_reddit, "id_numeric") %in% training)
dfmat_val <- dfm_subset(dfm, docvars(corpus_reddit, "id_numeric") %in% validation)

# Check if dfmat_train has non-zero features
if (sum(colSums(dfmat_train) > 0) == 0) {
  stop("dfmat_train has only zero-variance features. Adjust dfm_trim or review preprocessing steps.")
}



# Lasso Regression (alpha = 1 for Lasso)
# Run glmnet on the training DFM
lasso.1 <- glmnet(as.matrix(dfmat_train), docvars(dfmat_train, "Ann_Code"),
                  family = "multinomial", alpha = 1)

# Inspect lambda values and coefficients (optional)
print(lasso.1$lambda)
summary(lasso.1$beta)

# Different lambdas used by glmnet
lasso.1$lambda
# Example of beta summaries for different lambdas
summary(lasso.1$beta[[1]][,1])  # Coefficients for first lambda
summary(lasso.1$beta[[1]][,40]) # Coefficients for 40th lambda

# Sort the largest positive and smallest negative coefficients for the 40th lambda
sort(lasso.1$beta[[1]][,40], decreasing = TRUE)[1:40]
sort(lasso.1$beta[[1]][,40], decreasing = FALSE)[1:40]

# Performance out of sample
# Predict using the validation dfm
predict.test <- predict(lasso.1, as.matrix(dfmat_val), type="class")

# Evaluate with a confusion matrix
conf_matrix40 <- confusionMatrix(factor(predict.test[,40]), 
                                 factor(as.numeric(docvars(dfmat_val, "Ann_Code"))),
                                 mode = "prec_recall", positive = "1") # Positive class as "1"
print(conf_matrix40)

# Alternative lambda example
conf_matrix50 <- confusionMatrix(factor(predict.test[,50]), 
                                 factor(as.numeric(docvars(dfmat_val, "Ann_Code"))),
                                 mode = "prec_recall", positive = "1") # Positive class as "1"
print(conf_matrix50)






#Naive Bayes
#Train
tmod_nb <- textmodel_nb(dfmat_train, docvars(dfmat_train, "Ann_Code"))
summary(tmod_nb)

#Probability of a word given a category
coef_nb <- coef(tmod_nb)
head(coef_nb)

#Words associated with Ann_Code:
sort(coef_nb[,2]/coef_nb[,1], decreasing=T)[1:20]
#Words not associated with Ann_Code
sort(coef_nb[,2]/coef_nb[,1], decreasing=F)[1:20]

#How well does it do in sample?
predict.train <- predict(tmod_nb, dfmat_train)

tab_train <- table(docvars(dfmat_train, "Ann_Code"), predict.train)
tab_train

#precision
diag(tab_train)/colSums(tab_train)
#recall
diag(tab_train)/rowSums(tab_train)

#How well does this prediction do out of sample?  Validation
predict.val <- predict(tmod_nb, newdata = dfmat_val)

tab_val <- table(docvars(dfmat_val, "Ann_Code"), predict.val)
tab_val

#precision
diag(tab_val)/colSums(tab_val)
#recall
diag(tab_val)/rowSums(tab_val)

#Use confusion matrix to calculate F1
conf_matrix <- confusionMatrix(factor(predict.val), 
                               factor(docvars(dfmat_val, "Ann_Code")),
                               mode="prec_recall", positive="1")
print(conf_matrix)






# Trees
library(rpart)

# Trim the DFM to reduce sparsity
dfm_trim <- dfm_trim(dfm, min_docfreq = 0.0001, docfreq_type = "prop")

# Convert the trimmed DFM to a data frame and remove document IDs
dfmmat <- convert(dfm_trim, to = "data.frame")[, -1]

# Ensure unique and valid column names again, this time with more rigor
colnames(dfmmat) <- make.unique(make.names(colnames(dfmmat), unique = TRUE))

# Add `Ann_Code` as a column to `dfmmat`
dfmmat$Ann_Code <- docvars(dfm_trim, "Ann_Code")

# Train the decision tree model using the `training` subset
tree_model <- rpart(Ann_Code ~ ., data = dfmmat, subset = training, method = "class")


#Let's look at this tree
summary(tree_model)
plot(tree_model)
text(tree_model, pretty = 0)

#Let's see how it does on the test set
dfmmat_test <- dfmmat[-training,]
tree.pred <- predict(tree_model, dfmmat_test,
                     type = "class")

tab_val <- table(tree.pred, docvars(dfmat_val, "Ann_Code"))

#precision
diag(tab_val)/colSums(tab_val)
#recall
diag(tab_val)/rowSums(tab_val)

#Confusion matrix
conf_matrix <- confusionMatrix(factor(tree.pred), 
                               factor(docvars(dfmat_val, "Ann_Code")),
                               mode="prec_recall", positive="1")
print(conf_matrix)



##Random Forest
# Load necessary libraries
library(randomForest)
library(caret)

# Fit the random forest model
# Calculate `mtry` dynamically as the square root of the number of features in `dfmmat`
num_features <- ncol(dfmmat) - 1  # Subtract 1 to exclude `Ann_Code` column
rf.attitude <- randomForest(Ann_Code ~ ., data = dfmmat,
                            subset = training, mtry = sqrt(num_features), 
                            importance = TRUE)

# Make predictions on the validation set
# Use the validation subset of `dfmmat` for consistent indexing
dfmmat_val <- dfmmat[-training, ]
yhat.rf <- predict(rf.attitude, newdata = dfmmat_val)

# Calculate confusion matrix using a specified threshold (adjust if needed)
# Here, we assume `yhat.rf` outputs probabilities or scores
# Set an appropriate threshold for classification
threshold <- 0.5
predicted_labels <- factor(ifelse(yhat.rf > threshold, 1, 0), levels = c(-1, 0, 1))
actual_labels <- factor(docvars(dfmat_val, "Ann_Code"), levels = c(-1, 0, 1))

# Calculate the confusion matrix
conf_matrix <- confusionMatrix(predicted_labels, actual_labels, mode = "prec_recall", positive = "1")
print(conf_matrix)

# Calculate variable importance
var_importance <- importance(rf.attitude)

# Plot variable importance
varImpPlot(rf.attitude)

# Additional Analysis: Display top 20 most important features
top_features <- head(sort(var_importance[, "%IncMSE"], decreasing = TRUE), 20)
print("Top 20 Most Important Features by %IncMSE:")
print(top_features)





#####
#Cross Validation
#####

#Cross-validation with Lasso
cv <- cv.glmnet(dfmat_train, docvars(dfmat_train, "Ann_Code"),
                family="multinomial", alpha=1, 
                type.measure="mse")
plot(log(cv$lambda), cv$cvm, xlab="Log Lambda", ylab="Mean Cross-Validated Error")

#Predict for the test set
predict.test <- predict(cv, dfmat_val, type="class")

#How many comments are positive ?
prop.table(table(predict.test))
#What is the truth?
prop.table(table(docvars(dfmat_val, "Ann_Code")))

#Confusion matrix
conf_matrix <- confusionMatrix(factor(predict.test), 
                               factor(docvars(dfmat_val, "Ann_Code")),
                               mode="prec_recall", positive="1")

print(conf_matrix)


#Let's look at some of the predicted
# Check which classes have predictions
class_counts <- table(predict.test)

# Sample based on available classes
if ("1" %in% names(class_counts)) {
  sample(test_texts[predict.test == 1], min(10, class_counts["1"]))
} else {
  cat("No predictions for Class 1\n")
}

if ("0" %in% names(class_counts)) {
  sample(test_texts[predict.test == 0], min(10, class_counts["0"]))
} else {
  cat("No predictions for Class 0\n")
}

if ("-1" %in% names(class_counts)) {
  sample(test_texts[predict.test == -1], min(10, class_counts["-1"]))
} else {
  cat("No predictions for Class -1\n")
}




#########################
#Predictability as Measurement
#########################


# Predictability as Measurement
predict.test <- predict(cv, dfm, type = "response")
reddit$immigration_score <- predict.test
hist(reddit$immigration_score, main = "Predicted Immigration Attitude Score", xlim = range(reddit$immigration_score))
