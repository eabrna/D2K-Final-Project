---
title: "DTK Project Tree Models"
output: html_document
date: "2024-11-27"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(rpart)
library(rvest)
library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
```

```{r}
train <- read.csv("C:/Users/Mia/Downloads/Train (1).csv")
test <- read.csv("C:/Users/Mia/Downloads/Test (1).csv")

```

```{r}
train <- subset(train, select = -c(Sequence,Label))
test <- subset(test, select = -c(Sequence,Label))
train <- train[complete.cases(train),]
test <- test[complete.cases(test),]
train <- train %>% mutate(across(where(is.character), as.factor))
test <- test %>% mutate(across(where(is.character), as.factor))


```

```{r}
set.seed(1)

# Train a bagging model
bag_model <- randomForest(
  Activity ~ Size + BomanI + NetCharge + HydrophobicRatio + HydrophobicMoment + Aliphatic + InstaIndex + IsoelectricPoint,
  data = train,
  mtry = ncol(train) - 1,  # Use all predictors
  importance = TRUE
)

# Print the model summary
print(bag_model)
```
```{r}
bag_pred <- predict(bag_model, newdata = test)

# Evaluate the model
bag_tabel <- table(test$Activity, bag_pred)
bag_accuracy <- sum(diag(bag_tabel)) / sum(bag_tabel) * 100
print(bag_accuracy)

conf_matrix <- table(Predicted = bag_pred, Actual = test$Activity)
conf_df <- as.data.frame(conf_matrix)

# Plot the confusion matrix as a heatmap
ggplot(conf_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix: Predicted vs Actual",
       x = "Predicted Category",
       y = "Actual Category") +
  theme_minimal() +
  geom_text(aes(label = Freq), color = "black")
```
The confusion matrix plot shows the distribution of predicted and actual categories (non-microbial vs microbial). Darker cells indicate higher agreement or disagreement frequencies, helping to identify where the model performs well or struggles.

```{r}
pred_numeric <- as.numeric(bag_pred)
actual_numeric <- as.numeric(test$Activity)

# Create a data frame for plotting
plot_df <- data.frame(Predicted = pred_numeric, Actual = actual_numeric)

# Add a column to indicate match/difference
plot_df$Match <- ifelse(plot_df$Predicted == plot_df$Actual, "Match", "Differ")

# Plot with ggplot2
ggplot(plot_df, aes(x = Predicted, y = Actual, color = Match)) +
  geom_jitter(width = 0.2, height = 0.2, size = 3, alpha = 0.6) +
  scale_color_manual(values = c("Match" = "green", "Differ" = "red")) +
  labs(title = "Prediction vs Actual: Match vs Difference",
       x = "Predicted (Non-microbial = 1, Microbial = 2)",
       y = "Actual (Non-microbial = 1, Microbial = 2)") +
  theme_minimal()
```
The scatter plot highlights individual predictions, with green points indicating cases where the model correctly predicted the category (match) and red points showing mismatches (differ). This visualizes how often and where the model diverges from the true values.

```{r}
set.seed(42)

# Train the Random Forest model
rf_model <- randomForest(
  Activity ~ Size + BomanI + NetCharge + HydrophobicRatio + HydrophobicMoment + 
    Aliphatic + InstaIndex + IsoelectricPoint,
  data = train,
  ntree = 500,          # Number of trees
  mtry = floor(sqrt(ncol(train) - 1)),  # Number of predictors at each split
  importance = TRUE     # Calculate variable importance
)

# Print model summary
print(rf_model)

```

```{r}
# Make predictions on the test set
rf_pred <- predict(rf_model, newdata = test)

# Create a confusion matrix
conf_matrix_rf <- table(Predicted = rf_pred, Actual = test$Activity)

# Calculate accuracy
rf_accuracy <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf) * 100
print(paste("Random Forest Accuracy:", round(rf_accuracy, 2), "%"))

# Plot confusion matrix
library(ggplot2)
conf_df_rf <- as.data.frame(conf_matrix_rf)

ggplot(conf_df_rf, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(title = "Random Forest Confusion Matrix",
       x = "Predicted Category",
       y = "Actual Category") +
  theme_minimal() +
  geom_text(aes(label = Freq), color = "black")
```
```{r}
varImpPlot(rf_model)
```
Most important features: HydrophobicMoment, Size, NetCharge

```{r}
library(gbm)
train_gbm <- train[, c("Activity", "HydrophobicMoment", "Size", "NetCharge")]
test_gbm <- test[, c("Activity", "HydrophobicMoment", "Size", "NetCharge")]

# Make sure the target variable is a factor (for classification problems)
train_gbm$Activity <- as.numeric(train_gbm$Activity) - 1  # "microbial" = 1, "non-microbial" = 0
test_gbm$Activity <- as.numeric(test_gbm$Activity) - 1
```
```{r}
set.seed(42)  # For reproducibility

gbm_model <- gbm(
  Activity ~ HydrophobicMoment + Size + NetCharge,
  data = train_gbm,
  distribution = "bernoulli",  # For binary classification
  n.trees = 1000,              # Number of trees
  shrinkage = 0.01,            # Learning rate (adjust as needed)
  interaction.depth = 3,       # Depth of trees
  cv.folds = 5,                # Cross-validation folds to prevent overfitting
  n.cores = 1,                 # Number of cores (use more if available)
  verbose = TRUE               # To display the training progress
)

# Print the model summary
summary(gbm_model)
```
```{r}
# Predict on the test data
gbm_pred <- predict(gbm_model, newdata = test_gbm, n.trees = 1000, type = "response")

# Convert probabilities to class labels (0.5 threshold)
gbm_pred_class <- ifelse(gbm_pred > 0.5, 1, 0)

# Create a confusion matrix
conf_matrix_gbm <- table(Predicted = gbm_pred_class, Actual = test_gbm$Activity)

# Calculate accuracy
gbm_accuracy <- sum(diag(conf_matrix_gbm)) / sum(conf_matrix_gbm) * 100
print(paste("GBM Model Accuracy:", round(gbm_accuracy, 2), "%"))

# Plot the feature importance
plot(gbm_model, i.var = c("HydrophobicMoment", "Size", "NetCharge"))
```
The final figure shows the feature importance of HydrophobicMoment, Size, and NetCharge, indicating how much each feature contributes to the predictions made by the Gradient Boosting Machine (GBM) model, with higher values representing greater importance.
