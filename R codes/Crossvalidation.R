library(caret)
library(mlbench) # For a sample dataset

ctrl <- trainControl(method = "cv",
                     number = 10)  

# Train the logistic regression model using leave-one-out cross-validation
model <- train(Attrition ~ .,                   # Formula: response ~ predictors
               data = Train_scaled,                 # Training dataset
               method = "glm",                 # Logistic regression method
               trControl = ctrl,
               family= "binomial")               # Control parameters for cross-validation
                                               # Specify binomial family for logistic regression

# Compute the error
1 - model$results$Accuracy

# Print the error
print(error)
