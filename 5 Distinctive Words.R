# Load necessary libraries
library(tidyverse)
library(quanteda)

# Set working directory
setwd("/Users/azure/Desktop/Coursework/PhD Coursework/Text as Data/Homework/My Script/Measurement Memo II")

# Load the classified data
classified_data <- read_csv("classified_unlabeled_data.csv")

# Preprocess the 'Predicted_Label' column to ensure consistency
classified_data$Predicted_Label <- as.numeric(classified_data$Predicted_Label)

# Create a corpus
corpus_data <- corpus(classified_data, text_field = "comment_body")

# Tokenization and preprocessing
toks <- tokens(corpus_data, remove_punct = TRUE, remove_numbers = TRUE)  # Tokenize
toks <- tokens_select(toks, stopwords("en"), selection = "remove")  # Remove stop words

# Create the document-feature matrix (DFM)
dfm <- dfm(toks)

# Filter the DFM for documents with positive and negative labels
label_filter <- classified_data$Predicted_Label %in% c(1, -1)
dfm_filtered <- dfm[label_filter, ]

# Trim the DFM to only include words that appear in more than 1% of the documents
dfm_trimmed <- dfm_trim(dfm_filtered, min_docfreq = 0.0001, docfreq_type = "prop")

# Define a function for mutual information
mi <- function(dfm, clust.vect) {
  np <- sum(clust.vect)
  ns <- sum(!clust.vect)
  D <- np + ns
  nj <- apply(dfm, 2, function(x) sum(x > 0))
  nnotj <- apply(dfm, 2, function(x) sum(x == 0))
  njp <- apply(dfm[clust.vect, ], 2, function(x) sum(x > 0))
  njs <- apply(dfm[!clust.vect, ], 2, function(x) sum(x > 0))
  nnotjp <- apply(dfm[clust.vect, ], 2, function(x) sum(x == 0))
  nnotjs <- apply(dfm[!clust.vect, ], 2, function(x) sum(x == 0))
  
  mi <- njp / D * log((njp * D) / (np * nj), 2) +
    njs / D * log((njs * D) / (nj * ns), 2) +
    nnotjp / D * log((nnotjp * D) / (np * nnotj), 2) +
    nnotjs / D * log((nnotjs * D) / (nnotj * ns), 2)
  
  names(mi) <- colnames(dfm)
  return(mi)
}

# Calculate mutual information for positive and negative labels
mi_positive <- mi(dfm_trimmed, classified_data$Predicted_Label[label_filter] == 1)

# Calculate proportion of occurrence for positive and negative labels
np <- sum(classified_data$Predicted_Label[label_filter] == 1)
ns <- sum(classified_data$Predicted_Label[label_filter] == -1)

njp <- apply(dfm_trimmed[classified_data$Predicted_Label[label_filter] == 1, ], 2, function(x) sum(x > 0))
njs <- apply(dfm_trimmed[classified_data$Predicted_Label[label_filter] == -1, ], 2, function(x) sum(x > 0))

# Plot mutual information
plot(njp / np - njs / ns, mi_positive,
     col = "white",
     ylab = "Mutual Information",
     xlab = "Negative <--> Positive",
     main = "",
     cex.axis = 1.2,
     cex.lab = 1.5)

text(njp / np - njs / ns, mi_positive, names(mi_positive), cex = mi_positive / max(mi_positive, na.rm = TRUE) + 0.3)

# Get the top distinctive words for positive and negative labels
positive_words <- sort(mi_positive[njp / np - njs / ns > 0], decreasing = TRUE)[1:30]
negative_words <- sort(mi_positive[njp / np - njs / ns < 0], decreasing = TRUE)[1:30]

# Display results
cat("Distinctive words for Positive Label:\n")
print(positive_words)

cat("Distinctive words for Negative Label:\n")
print(negative_words)
