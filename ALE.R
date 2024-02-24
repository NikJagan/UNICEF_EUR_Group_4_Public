library(ALEPlot)
library(randomForest)
library(ale)
library(dplyr)

#ALE####
setwd("/Users/marleenhaubitz/Documents/Erasmus University/DSMA/UNICEF")
#load("/Users/marleenhaubitz/Documents/Erasmus University/DSMA/UNICEF/RandomForest3")
load("/Users/marleenhaubitz/Downloads/RandomForest6.RData")

#suddenly I needed package ranger I don't know why.. 
predictions <- as.data.frame(predict(tuned_model, df_test))
colnames(predictions)[1] <- "online_donation_count"

#Adjust Variable Names to see if code works
test_data <- df_test


#did not run this because the train data the rf model was trained on contains factors
#if I transform I get an error in ale_ixn
con <- function(df) {
	# Get column names
	col_names <- names(df)

	# Loop through each column
	for (col in col_names) {
		# Convert to numeric if not already numeric
		if (!is.numeric(df[[col]])) {
			df[[col]] <- as.numeric(df[[col]])
		}
	}

	return(df)
}
#test_data <- con(test_data)

ale_test_data <- test_data
variable_names <- names(ale_test_data)
ale_test_data[, "online_donation_count"] <- NULL
ale_test_data_with_probs <- cbind(ale_test_data, predictions)
variable_names <- names(ale_test_data)

#custom_predict <- function(object, newdata) {
	#as.data.frame(predict(object, newdata, type = "prob"))$"1"
#}
#a <- names(df_train[, 2:16])
#b <- names(df_train[, 17:30])
#ale_test_data_with_probs_test <- ale_test_data_with_probs
#ale_test_data_with_probs_test <- transform(ale_test_data_with_probs_test, Nederland_ind = as.integer(Nederland_ind))
 
ale_ixn <- ale_ixn(
	ale_test_data_with_probs, 
	tuned_model, 
	pred_fun = function(object, newdata) {
		predict(object, newdata, type = "raw")
	}, 
	silent = FALSE,
	x1_cols = "Amerika", 
	
	x2_cols = "Europe_ind",
	
	y_col = "online_donation_count", 
	relative_y = 'median',
	y_type =  'numeric',
	x_intervals = 100,
	median_band = 0.05,
)


directory_path <- "/Users/marleenhaubitz/Documents/Erasmus University/DSMA/UNICEF/Plots/result_second_order"
dir.create(directory_path, recursive = TRUE)

#this does not print the plot 
pdf(file=str_glue('result_second_order/plot_.pdf'))
print(ale_ixn[["plots"]][["Afrika"]][["Bevolking"]])
dev.off()

pdf(file=str_glue('result_second_order/plot_.pdf'))
print(ale_ixn[["plots"]][["Bevolking"]][["Suriname"]])
dev.off()




