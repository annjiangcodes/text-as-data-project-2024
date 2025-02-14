# Load necessary libraries
library(tidyverse)
library(readr)

# Set working directory. Update the path as needed.
setwd("/Users/azure/Desktop/Coursework/PhD Coursework/Text as Data/Homework/My Script/Representation Memo Data")

# Load data for speeches
metadata <- read_csv("Appendix 4 Merged_Reddit_Data.csv")

# Look at data (optional, for verification)
print(metadata)

# Randomly select a training set of 100 documents
set.seed(123)  # Set seed for reproducibility
training_set <- metadata %>% sample_n(100)

# View the randomly selected training set (optional)
print(training_set)
view (training_set)

# Create a spreadsheet of the randomly selected subsample
write_csv(training_set, "random_training_set.csv")

# Incorporate the subsample spreadsheet back into R
# Read the subsample back into R
subsample <- read_csv("random_training_set.csv")

# View the subsample data (optional)
print(subsample)




# Randomly select another training set of 100 documents
set.seed(456)  # Set a different seed for reproducibility
new_training_set <- metadata %>% sample_n(100)

# View the randomly selected new training set (optional)
print(new_training_set)
view(new_training_set)

# Create a spreadsheet of the new randomly selected subsample
write_csv(new_training_set, "new_random_training_set.csv")

# Incorporate the new subsample spreadsheet back into R
# Read the new subsample back into R
new_subsample <- read_csv("new_random_training_set.csv")

# View the new subsample data (optional)
print(new_subsample)


# Install irr package if not already installed
if (!require(irr)) {
  install.packages("irr")
}

# Load the irr package
library(irr)

#Let's look at intercoder reliability
#handcoding <- 
random_training_set_hand_coding_merged <- read.csv("/Users/azure/Desktop/Coursework/PhD Coursework/Text as Data/Homework/My Script/Measurement I Coding/random_training_set_hand_coding_merged.csv")

#Confusion Matrix
table(random_training_set_hand_coding_merged$Ann, random_training_set_hand_coding_merged$Sam)
#Krippendorff's alpha
kripp.alpha(t(random_training_set_hand_coding_merged[,c("Ann", "Sam")]))

head(random_training_set_hand_coding_merged[random_training_set_hand_coding_merged$Ann != random_training_set_hand_coding_merged$Sam, c("comment_body", "Ann", "Sam")])

