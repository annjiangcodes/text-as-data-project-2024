# Load necessary libraries
library(tidyverse)
library(quanteda)
library(glmnet)

# Set working directory and load unlabeled data
setwd("/Users/azure/Desktop/Coursework/PhD Coursework/Text as Data/Homework/My Script/Representation Memo Data")
unlabeled_data <- read_csv("Appendix 4 Merged_Reddit_Data.csv")  # Replace with your file name

# Preprocess unlabeled data
# Create a corpus for the unlabeled data
corpus_unlabeled <- corpus(unlabeled_data, text_field = "comment_body")

# Tokenize and create a DFM (matching preprocessing from training)
toks_unlabeled <- tokens(corpus_unlabeled, remove_punct = TRUE, remove_numbers = TRUE)
toks_unlabeled <- tokens_select(toks_unlabeled, stopwords("en"), selection = "remove")
dfm_unlabeled <- dfm(toks_unlabeled)

# Align features of the unlabeled DFM with the training DFM
dfm_unlabeled <- dfm_match(dfm_unlabeled, features = featnames(dfmat_train))

# Check if dfm_unlabeled has non-zero features
if (sum(colSums(dfm_unlabeled) > 0) == 0) {
  stop("dfm_unlabeled has only zero-variance features. Adjust dfm_trim or review preprocessing steps.")
}

# Load the trained Lasso model (already trained on dfmat_train)
# Assuming `lasso.1` contains the trained model

# Select the best lambda (e.g., based on validation performance)
best_lambda <- lasso.1$lambda[40]  # Replace 40 with your best-performing lambda index

# Predict labels for the unlabeled data
predictions <- predict(lasso.1, as.matrix(dfm_unlabeled), s = best_lambda, type = "class")

# Add predictions to the unlabeled dataset
unlabeled_data$Predicted_Label <- as.numeric(predictions)

# Save the classified data to a CSV file
write_csv(unlabeled_data, "classified_unlabeled_data.csv")

# Print summary of predictions
print(table(unlabeled_data$Predicted_Label))
