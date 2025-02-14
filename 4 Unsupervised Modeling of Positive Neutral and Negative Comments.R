# Load necessary libraries
library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(stm)
library(tokenizers)
library(seededlda)
library(ggplot2)

# Set working directory
setwd("/Users/azure/Desktop/Coursework/PhD Coursework/Text as Data/Homework/My Script/Measurement Memo II")

# Load the classified data
classified_data <- read_csv("classified_unlabeled_data.csv")

# Filter data for labels 1, 0, and -1
data_positive <- classified_data %>% filter(Predicted_Label == 1)
data_negative <- classified_data %>% filter(Predicted_Label == -1)
data_neutral <- classified_data %>% filter(Predicted_Label == 0)

# Function for preprocessing and creating DFM
create_dfm <- function(data, text_field) {
  corpus_data <- corpus(data, text_field = text_field)
  toks <- tokens(corpus_data, remove_punct = TRUE, remove_numbers = TRUE)
  toks <- tokens_select(toks, stopwords("en"), selection = "remove")
  dfm <- dfm(toks)
  dfm_trimmed <- dfm_trim(dfm, min_docfreq = 0.001, docfreq_type = "prop")
  return(dfm_trimmed)
}

# Create DFMs for positive, negative, and neutral labels
dfm_positive <- create_dfm(data_positive, "comment_body")
dfm_negative <- create_dfm(data_negative, "comment_body")
dfm_neutral <- create_dfm(data_neutral, "comment_body")

# Function for running LDA
run_lda <- function(dfm, k) {
  lda <- textmodel_lda(dfm, k = k)
  lda_terms <- terms(lda, 10)
  list(lda_model = lda, lda_terms = lda_terms)
}

# Run LDA for k = 5, 10, 15 for positive, negative, and neutral labels
k_values <- c(5, 10, 15)
lda_results <- list()

for (k in k_values) {
  cat(paste("\nRunning LDA for k =", k, "...\n"))
  
  lda_results[[paste0("positive_k", k)]] <- run_lda(dfm_positive, k)
  lda_results[[paste0("negative_k", k)]] <- run_lda(dfm_negative, k)
  lda_results[[paste0("neutral_k", k)]] <- run_lda(dfm_neutral, k)
  
  cat("LDA terms for positive label (k =", k, "):\n")
  print(lda_results[[paste0("positive_k", k)]]$lda_terms)
  
  cat("LDA terms for negative label (k =", k, "):\n")
  print(lda_results[[paste0("negative_k", k)]]$lda_terms)
  
  cat("LDA terms for neutral label (k =", k, "):\n")
  print(lda_results[[paste0("neutral_k", k)]]$lda_terms)
}

###### Function for running STM
run_stm <- function(data, k) {
  temp <- textProcessor(documents = data$comment_body, metadata = data)
  out <- prepDocuments(temp$documents, temp$vocab, temp$meta)
  stm_model <- stm(out$documents, out$vocab, K = k, prevalence = ~1, data = out$meta, max.em.its = 50)
  list(stm_model = stm_model, prepared_data = out)
}

# Run STM for k = 5, 10, 15 for positive, negative, and neutral labels
stm_results <- list()

for (k in k_values) {
  cat(paste("\nRunning STM for k =", k, "...\n"))
  
  stm_results[[paste0("positive_k", k)]] <- run_stm(data_positive, k)
  stm_results[[paste0("negative_k", k)]] <- run_stm(data_negative, k)
  stm_results[[paste0("neutral_k", k)]] <- run_stm(data_neutral, k)
  
  cat("STM topics for positive label (k =", k, "):\n")
  print(labelTopics(stm_results[[paste0("positive_k", k)]]$stm_model))
  
  cat("STM topics for negative label (k =", k, "):\n")
  print(labelTopics(stm_results[[paste0("negative_k", k)]]$stm_model))
  
  cat("STM topics for neutral label (k =", k, "):\n")
  print(labelTopics(stm_results[[paste0("neutral_k", k)]]$stm_model))
}




