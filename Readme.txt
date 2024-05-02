---------Dataset Link-------------
https://www.kaggle.com/datasets/petalme/seattle-weather-prediction-dataset
----------------

# Assuming 'mydata' is your dataset with significant attributes and 'target' is the target variable
# Assuming you have already preprocessed your data and encoded categorical variables

# Load the required library
library(e1071)

# Approach 1: Dividing the data into training and test set
set.seed(435)
sample_index <- sample(1:nrow(dataset1), 0.8 * nrow(dataset1))
train_data <- dataset1mydata[sample_index, ]
test_data <- dataset1[-sample_index, ]

# Create a Naive Bayes model using training data
nb_model_train <- naiveBayes(target ~ ., data = train_data)

# Make predictions on the test set
predictions_test <- predict(nb_model_train, test_data)

# Calculate and print the accuracy on the test set
accuracy_test <- sum(predictions_test == test_data$target) / nrow(test_data)
cat("Accuracy on the test set:", accuracy_test, "\n")

