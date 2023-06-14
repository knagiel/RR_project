# Load necessary libraries
library(tidyverse)
library(caret)
library(glmnet)
library(gplots)
#load the dataset
data <- read_csv("diabetes2.csv")

# Check for missing values
colSums(is.na(data))

# Explore the dataset
str(data)
summary(data)
cor(data)


# Split the dataset into x and y variables
features <- c('Pregnancies', 'Glucose', 'BloodPressure', 'SkinThickness', 'Insulin', 'BMI', 'DiabetesPedigreeFunction', 'Age')
x <- data[features]
y <- data$Outcome

# Split the dataset into training and testing sets
set.seed(0)
train_indices <- createDataPartition(y, p = 0.9, list = FALSE)
x_train <- x[train_indices, ]
x_test <- x[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

# Train a logistic regression model
reg <- glmnet(as.matrix(x_train), y_train, family = 'binomial')

# Convert predicted labels to factor with correct levels
y_pred <- factor(ifelse(y_pred_prob > 0.5, 1, 0), levels = c(0, 1))
y_test <- factor(y_test, levels = c(0, 1))

# Calculate the confusion matrix manually
confusion_matrix <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("Actual 0", "Actual 1"), c("Predicted 0", "Predicted 1")))

# Iterate over each pair of true and predicted labels
for (i in 1:length(y_test)) {
  true_label <- y_test[i]
  pred_label <- y_pred[i]
  
  # Increment the corresponding cell in the confusion matrix
  confusion_matrix[true_label, pred_label] <- confusion_matrix[true_label, pred_label] + 1
}

# Print the confusion matrix
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy))

# Save the model
saveRDS(reg, file = "logistic_reg.rds")

# User-defined data
new_data <- data.frame(Pregnancies = 5, Glucose = 0, BloodPressure = 33.7, 
                       SkinThickness = 50, Insulin = 150, BMI = 74, 
                       DiabetesPedigreeFunction = 0.5, Age = 53)

# Load the saved model
loaded_model <- readRDS("logistic_reg.rds")

# Predict on new data
ourmodelprediction <- predict(loaded_model, data = new_data, type = 'class')
ourmodelprediction
