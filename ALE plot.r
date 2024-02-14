#read the model 
X.model <-
newdata <- fread("datasets/full_data_test.csv", sep = ",")
X <- fread("datasets/full_data_train.csv", sep = ",")
X$donation_count <- NA 

yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))

#create an ALE plot
ALEPlot(X, X.model, pred.fun, J=c(1,2), K = 40, NA.plot = F)
