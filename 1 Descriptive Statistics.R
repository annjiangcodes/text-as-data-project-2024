# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Set working directory
setwd("/Users/azure/Desktop/Coursework/PhD Coursework/Text as Data/Homework/My Script/Measurement Memo II")

# Load the classified data
classified_data <- read_csv("classified_unlabeled_data.csv")

# View the first few rows
head(classified_data)

# Check the structure of the data
str(classified_data)

# Summary statistics
summary(classified_data)

# Count the number of documents# Calculate the length of each document (word count)
classified_data <- classified_data %>%
  mutate(doc_length = str_count(comment_body, "\\S+")) # Count non-whitespace characters

# Summary of document lengths
summary(classified_data$doc_length)

# Plot distribution of document lengths
ggplot(classified_data, aes(x = doc_length)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Document Lengths", x = "Word Count", y = "Frequency")

num_docs <- nrow(classified_data)
print(paste("Number of documents:", num_docs))



# Count the distribution of predicted labels
label_distribution <- classified_data %>%
  count(Predicted_Label)

# Display the distribution table
print(label_distribution)

# Plot label distribution
ggplot(label_distribution, aes(x = as.factor(Predicted_Label), y = n)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(title = "Distribution of Predicted Labels", x = "Predicted Label", y = "Count") +
  theme_minimal()

# Boxplot of document length by predicted label
ggplot(classified_data, aes(x = as.factor(Predicted_Label), y = doc_length)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Document Length by Predicted Label", x = "Predicted Label", y = "Word Count") +
  theme_minimal()

# Plot number of comments by creation date
classified_data %>%
  mutate(comment_date = as.Date(comment_created_utc)) %>%
  group_by(comment_date) %>%
  summarize(num_comments = n()) %>%
  ggplot(aes(x = comment_date, y = num_comments)) +
  geom_line(color = "blue") +
  labs(title = "Number of Comments Over Time", x = "Date", y = "Number of Comments") +
  theme_minimal()


# Count the distribution of places
place_distribution <- classified_data %>%
  count(place)

# Display the place distribution table
print(place_distribution)

# Plot place distribution
ggplot(place_distribution, aes(x = reorder(place, -n), y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Distribution of Places", x = "Place", y = "Count") +
  theme_minimal()

# Plot distribution of comment scores
ggplot(classified_data, aes(x = comment_score)) +
  geom_histogram(binwidth = 50, fill = "red", color = "black") +
  labs(title = "Distribution of Comment Scores", x = "Score", y = "Frequency") +
  theme_minimal()
