library(dplyr)
library(caret)
bd_data <- read.csv("C:\\Users\\Dhair\\Downloads\\oulad-students.csv")
bd_data$final_result <- as.factor(bd_data$final_result)
set.seed(657) 
# For consistency
training_indices <- sample(1:nrow(bd_data), 0.7 * nrow(bd_data))
training_data <- bd_data[train_indices, ]
testing_data <- bd_data[-train_indices, ]
logit_model <- glm(final_result ~ ., data = training_data, family = binomial(link = "logit"))
testing_predictions <- predict(logit_model, newdata = testing_data, type = "response")
threshold <- 0.4
predicted_data <- ifelse(test_predictions > threshold, "Pass", "withdrawn")
actual_classes <- test_data$final_result
accuracy <- sum(predicted_classes == actual_classes) / length(actual_classes)
cat("Accuracy on test set:", accuracy, "\n")