library(ALEPlot)
library(randomForest)

#read the model 
load("RandomForest.RData")

#remove the dependent var
df_train <- subset(df_train, select = -online_donation_count )
df_test <- subset(df_test, select = -online_donation_count)

#define the prediction func
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))

names(df_train)
str(df_train)



#create ALE 1st order plots for most important vars
#pledges
ALEPlot(df_train, rf_model, pred.fun=yhat, J="pledge_ind",K=20, NA.plot = F)
#demographics
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Nederlandse.achtergrond",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Afrika",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Azië",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Europa..excl..Nederlandse.achtergrond.",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Oceanië",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="België",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Duitsland",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Indonesië",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Marokko",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="X.voormalige..Nederlandse.Antillen..Aruba",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Polen",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Suriname",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Turkije",K=20, NA.plot = F)
#external event
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Nederland_ind",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Europe_ind",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="World_ind",K=20, NA.plot = F)


#create ALE 2nd order plots for proximities

ALEPlot(df_train, rf_model, pred.fun=yhat, J=c(25,17),K=20, NA.plot = F)
