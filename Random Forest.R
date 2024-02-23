# Load the randomForest package
library(randomForest)
library(ggplot2)
library(Metrics)
library(caret)

# Load full_data_train.csv
full_data_train <- read.csv("full_data_train.csv")

# Load full_data_test.csv
full_data_test <- read.csv("full_data_test.csv")

df_train <- full_data_train[, !(names(full_data_train) %in% c("Year", "Week", "Postcode", "total_donations_count"))]

df_train$Nederland_ind <- as.factor(df_train$Nederland_ind)
df_train$Europe_ind <- as.factor(df_train$Europe_ind)
df_train$World_ind <- as.factor(df_train$World_ind)
df_train$Marokko_ind <- as.factor(df_train$Marokko_ind)
df_train$Turkije_ind <- as.factor(df_train$Turkije_ind)
df_train$Belgie_ind <- as.factor(df_train$Belgie_ind)
df_train$Duitsland_ind <- as.factor(df_train$Duitsland_ind)
df_train$Indonesie_ind <- as.factor(df_train$Indonesie_ind)
df_train$Polen_ind <- as.factor(df_train$Polen_ind)
df_train$Suriname_ind <- as.factor(df_train$Suriname_ind)
df_train$Oceanie_ind <- as.factor(df_train$Oceanie_ind)
df_train$Afrika_ind <- as.factor(df_train$Afrika_ind)
df_train$Amerika_ind <- as.factor(df_train$Amerika_ind)
df_train$Asie_ind <- as.factor(df_train$Asie_ind)
df_train$pledge_ind <- as.factor(df_train$pledge_ind)

# Removing '0' factor variables from df_train
df_train <- df_train[, !(names(df_train) %in% c("Marokko_ind", "Belgie_ind", "Duitsland_ind", "Polen_ind"))]

# Tuning Random Forest

# 1. Set up cross-validation
set.seed(123)  # For reproducibility
control <- trainControl(
  method = "cv",  # k-fold cross-validation
  number = 10,    # number of folds
  savePredictions = "final",  # Save predictions for the final model
  verboseIter = TRUE  # Print training log
)

# 2. Define the tuning grid
tuneGrid <- expand.grid(
  .mtry = c(3, 4, 5, 6), # Number of variables considered at each split
  .splitrule = c("extratrees"), # Splitting rule
  .min.node.size = c(1, 5, 10, 20)  # Minimum node size
)

# 3. Train the model
set.seed(123)  # For reproducibility
tuned_model <- train(
  online_donation_count ~ ., 
  data = df_train, 
  method = "ranger", 
  trControl = control, 
  tuneGrid = tuneGrid,
  importance = 'impurity'  # or 'permutation'
)

# Extracting feature importance
importance_values <- varImp(tuned_model, scale = FALSE)

# Converting to a data frame
feature_importance <- as.data.frame(importance_values$importance)

# Adding a column for feature names
feature_importance$Feature <- rownames(feature_importance)

# Selecting the importance column and renaming it
# Assuming the importance metric is %IncMSE, but check the actual name in your results
feature_importance <- feature_importance[, c("Feature", "Overall")]

# Sorting based on importance
feature_importance <- feature_importance[order(-feature_importance$Overall), ]

#select top 10
feature_importance <- feature_importance[1:10,]

# UNICEF blue color (approximation)
unicef_blue <- "#0099da"

# Plotting feature importance with UNICEF color scheme
p<- ggplot(feature_importance, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = unicef_blue) +
  coord_flip() + # for horizontal bars
  xlab("Features") +
  ylab("Importance") +
  ggtitle("Feature Importance in Random Forest Model") +
  theme(
    plot.title = element_text(color = unicef_blue, size = 14, face = "bold"),
    axis.title = element_text(color = "black", size = 12),
    axis.text = element_text(color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )    + theme_classic()

ggsave("fig7_feature_importance.png",p)

# Remove the 'Postcode', 'Year', and 'Week' columns from the test data
df_test <- full_data_test[, !(names(full_data_test) %in% c("Postcode", "Year", "Week", "total_donations_count"))]

df_test$Nederland_ind <- as.factor(df_test$Nederland_ind)
df_test$Europe_ind <- as.factor(df_test$Europe_ind)
df_test$World_ind <- as.factor(df_test$World_ind)
df_test$Marokko_ind <- as.factor(df_test$Marokko_ind)
df_test$Turkije_ind <- as.factor(df_test$Turkije_ind)
df_test$Belgie_ind <- as.factor(df_test$Belgie_ind)
df_test$Duitsland_ind <- as.factor(df_test$Duitsland_ind)
df_test$Indonesie_ind <- as.factor(df_test$Indonesie_ind)
df_test$Polen_ind <- as.factor(df_test$Polen_ind)
df_test$Suriname_ind <- as.factor(df_test$Suriname_ind)
df_test$Oceanie_ind <- as.factor(df_test$Oceanie_ind)
df_test$Afrika_ind <- as.factor(df_test$Afrika_ind)
df_test$Amerika_ind <- as.factor(df_test$Amerika_ind)
df_test$Asie_ind <- as.factor(df_test$Asie_ind)
df_test$pledge_ind <- as.factor(df_test$pledge_ind)

# Removing '0' factor variables from df_test
df_test <- df_test[, !(names(df_test) %in% c("Marokko_ind", "Belgie_ind", "Duitsland_ind", "Polen_ind"))]


# Predict using the Random Forest model
predictions <- predict(tuned_model, newdata = df_test)

# Actual values from the test set
actual_values <- df_test$online_donation_count

# Calculating RMSE
rmse_score <- rmse(actual_values, predictions)

# Printing the RMSE score
print(rmse_score)

# Calculate R-squared
r2_value <- postResample(pred = predictions, obs = actual_values)[["Rsquared"]]

# Print the R-squared value
print(r2_value)

save.image(file = "RandomForest.RData") # Saving the entire workspace

