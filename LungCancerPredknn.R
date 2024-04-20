# Load required libraries.
library(caret)
library(class)

# Load the data set.
data <- read.csv("/Users/annethsivakumar/Downloads/CancerPatientDataSets.csv")

# Set the seed for reproducibility.
set.seed(123)

# Generate a random index for splitting the data.
random_index <- sample(nrow(data), 500)

# Split the data into training and testing sets.
data_train <- data[random_index, ]
data_test <- data[-random_index, ]

# Convert 'Level' column to factor with the same levels in both data_train and data_test
levels <- unique(c(data_train$Level, data_test$Level))
data_train$Level <- factor(data_train$Level, levels = levels)
data_test$Level <- factor(data_test$Level, levels = levels)

# Normalize the data.
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
data_train_n <- as.data.frame(lapply(data_train[, -c(1, 2, 26)], normalize))
data_test_n <- as.data.frame(lapply(data_test[, -c(1, 2, 26)], normalize))

# Perform kNN classification.
knn_predicted <- knn(train = data_train_n, test = data_test_n,
                     cl = data_train$Level, k = 3)

# Evaluate the model.
confusionMatrix(data = knn_predicted, reference = data_test$Level)


library(ggplot2)

# Confusion matrix values
conf_matrix <- matrix(c(178, 0, 0,
                        0, 174, 0,
                        0, 0, 148), nrow = 3, byrow = TRUE,
                      dimnames = list(Reference = c("Medium", "High", "Low"),
                                      Prediction = c("Medium", "High", "Low")))

# Convert the matrix to a data frame for ggplot
conf_matrix_df <- as.data.frame(as.table(conf_matrix))
colnames(conf_matrix_df) <- c("Reference", "Prediction", "Count")

# Create the plot
ggplot(data = conf_matrix_df, aes(x = Reference, y = Prediction, fill = Count)) +
  geom_tile(color = "lightgrey") +
  geom_text(aes(label = Count)) +
  scale_fill_gradient(low = "lightblue", high = "blue", name = "Frequency") +
  theme_minimal() +
  labs(title = "Confusion Matrix",
       x = "True Label",
       y = "Predicted Label")

