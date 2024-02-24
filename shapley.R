setwd("/Users/marleenhaubitz/Documents/Erasmus University/DSMA/UNICEF")
load("RandomForest4.RData")
predictions <- as.data.frame(predict(tuned_model, df_test))
colnames(predictions)[1] <- "online_donation_count"

X <- df_train[, !(names(df_train)) %in% c("online_donation_count")]
X_test <- df_test[-which(names(df_test) == "online_donation_count")] 
response <- as.vector(df_train$online_donation_count)

#Create a "Predictor" object that holds model and data
predictor_all <- Predictor$new(tuned_model, data = X, y = response)

#Display and plot results
shapley_all <- Shapley$new(predictor_all, x.interest = X_test[2, ])
shapley_all
shapley_all$plot()


#PLOT THE DISTRIBUTION 
ggplot(data = predictions, aes(x = online_donation_count)) + 
	geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
	labs(x = "Online Donations Predictions")

ggplot(data = df_train, aes(x = online_donation_count)) + 
	geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
	labs(x = "Online Donations Train")



