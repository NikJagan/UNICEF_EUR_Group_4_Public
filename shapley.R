load("RandomForest.RData")

`%notin%` <- negate(`%in%`)

library(iml)


X <- df_train[, !(names(df_train)) %in% c("online_donation_count")]
X_test <- df_test[-which(names(df_test) == "online_donation_count")] 
response <- as.vector(df_train$online_donation_count)

predictions <- as.data.frame(predict(tuned_model, X_test))
colnames(predictions)[1] <- "pred_od_count"

#Create a "Predictor" object that holds model and data
predictor_all <- Predictor$new(tuned_model, data = X, y = response)

#Display and plot results
shapley_all <- Shapley$new(predictor_all, x.interest = X_test[3, ])
shapley_all
shapley_all$plot()+scale_fill_manual(values=c(rep(unicef_blue, 26)))


#PLOT THE DISTRIBUTION 
ggplot(data = predictions, aes(x = online_donation_count)) + 
	geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
	labs(x = "Online Donations Predictions")

ggplot(data = df_train, aes(x = online_donation_count)) + 
	geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
	labs(x = "Online Donations Train")


df_test$row <-  1:nrow(df_test)
 donation1 <- df_test[df_test$online_donation_count == 1, ]
 donation0 <- df_test[df_test$online_donation_count == 0, ]
 donation5 <- df_test[df_test$online_donation_count == 5, ]

 

 

 
 
 
