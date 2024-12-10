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
set.seed(42)

#Train random forest model
rf_model <- randomForest(
  Activity ~.,
  data = train,
  ntree = 500,        
  mtry = floor(sqrt(ncol(train) - 1)),  
  importance = TRUE    
)

print(rf_model)
```
```{r}
rf_pred <- predict(rf_model, newdata = test)

conf_matrix_rf <- table(Predicted = rf_pred, Actual = test$Activity)

rf_accuracy <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf) * 100
print(paste("Random Forest Accuracy:", round(rf_accuracy, 2), "%"))

library(ggplot2)
conf_df_rf <- as.data.frame(conf_matrix_rf)

ggplot(conf_df_rf, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "darkorange2", high = "dodgerblue4") +
  labs(title = "Random Forest Confusion Matrix",
       x = "Predicted Category",
       y = "Actual Category") +
  theme_minimal() +
  geom_text(aes(label = Freq), color = "black")
```

```{r}
varImpPlot(rf_model)
plot(rf_model, i.var = c("HydrophobicMoment", "Size", "NetCharge"))
```
Most important features: HydrophobicMoment, Size, NetCharge

```{r}
library(tree)
library(ggplot2)

# Train a decision tree model using the 'tree' function
set.seed(42)
dt_model_tree <- tree(Activity ~ ., data = train)
summary(dt_model_tree)

plot(dt_model_tree)
text(dt_model_tree, pretty = 0, cex = 0.8)

dt_pred_tree <- predict(dt_model_tree, newdata = test, type = "class")

conf_matrix_dt_tree <- table(Predicted = dt_pred_tree, Actual = test$Activity)

conf_df_dt_tree <- as.data.frame(conf_matrix_dt_tree)

ggplot(conf_df_dt_tree, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "darkorange2", high = "dodgerblue4") +
  labs(title = "Decision Tree Confusion Matrix",
       x = "Predicted Category",
       y = "Actual Category") +
  theme_minimal() +
  geom_text(aes(label = Freq), color = "black")


dt_tree_accuracy <- sum(diag(conf_matrix_dt_tree)) / sum(conf_matrix_dt_tree) * 100
print(paste("Decision Tree Accuracy (tree package):", round(dt_tree_accuracy, 2), "%"))


print(conf_matrix_dt_tree)

```

