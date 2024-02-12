setwd("C:/Users/nikil/Desktop/Unicef")
df1 <- read.csv("cleaned_joined_data.csv")
View(df1)

library(dplyr)
library(caret)

#Set a seed for reproducibility

set.seed(42)

# Get the total number of rows in the dataset
n <- nrow(df1)

# Create a random sample of row indices for the training set
trainIndex <- sample(1:n, size = floor(0.80 * n))

# Create the training and testing sets
train_set <- df1[trainIndex, ]
test_set <- df1[-trainIndex, ]

# Displaying the dimensions of the train and test sets to verify the split
dim(train_set)
dim(test_set)

