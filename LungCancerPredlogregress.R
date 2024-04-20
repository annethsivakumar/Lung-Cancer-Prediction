# Load required libraries.
library(tidyverse)

# Load the data set.
data <- read.csv("/Users/annethsivakumar/Downloads/CancerPatientDataSets.csv")

# Normalize the data.
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
data_normalized <- as.data.frame(lapply(data[, -c(1, 2, 26)], normalize))

# Add back the 'Level' column to the normalized data set.
data_normalized$Level <- data$Level

# Filter data to remove 'Medium' level in 'Level' variable
data_filtered <- data_normalized[data_normalized$Level != "Medium", ]

# Split the data into training and test set
set.seed(1)
random_index <- sample(nrow(data_filtered), size = nrow(data_filtered) * 0.7)
data_train <- data_filtered[random_index, ]
data_test <- data_filtered[-random_index, ]

# Convert 'Level' to a binary outcome
data_train$Level_binary <- ifelse(data_train$Level == "High", 1, 0)

# Fit logistic regression model
model <- glm(Level_binary ~., data = data_train, family = "binomial")

# Summary
summary(model)

# Predict using the model on the test set
predictions <- predict(model, newdata = data_test, type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_levels <- ifelse(predictions > 0.5, "High", "Low")

# Calculate accuracy
accuracy <- mean(predicted_levels == data_test$Level)
accuracy

# Create Confusion Matrix table
conf_matrix <- table(predicted_levels, data_test$Level)
conf_matrix

# Convert to data frame
conf_matrix_df <- as.data.frame(conf_matrix)
conf_matrix_df <- rename(conf_matrix_df, Predicted = predicted_levels, Actual = Var2)

# Create heatmap
ggplot(conf_matrix_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Confusion Matrix",
       x = "Actual",
       y = "Predicted",
       fill = "Frequency")


# Install and load the pROC package
library(pROC)

# Create ROC curve
roc_curve <- roc(data_test$Level, predictions)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve",
     col = "blue", lwd = 2,
     print.auc = TRUE, grid=c(0.1, 0.2, 0.5, 0.8, 0.9),
     print.thres = TRUE)

# Add AUC to the plot
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve), 2)),
       col = "blue", lwd = 2)









