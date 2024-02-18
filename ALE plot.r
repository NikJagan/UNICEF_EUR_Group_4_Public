library(ALEPlot)
library(randomForest)

#read the model 
load("RandomForest.RData")

#define the prediction func
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))

names(df_train)
str(df_train)

#create ALE 1st order plots for most important vars
#pledges
ALE_pledge <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="pledge_ind",K=20, NA.plot = F) 
plot(ALE_pledge$x.values, ALE_pledge$f.values, type="l", xlab="Presence of a pledge", ylab="Effect on DV", main="ALE main effect of pledges on online donations")
dev.copy(png,'pledges.png')
dev.off()

#demographics
ned <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Nederlandse.achtergrond",K=20, NA.plot = T)
afrika <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Afrika",K=20, NA.plot = F)
asia <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Azië",K=20, NA.plot = F)
europe <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Europa..excl..Nederlandse.achtergrond.",K=20, NA.plot = F)
oceania <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Oceanië",K=20, NA.plot = F)
Indonesia <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Indonesië",K=20, NA.plot = F)
morocco <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Marokko",K=20, NA.plot = F)
NedAnt <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="X.voormalige..Nederlandse.Antillen..Aruba",K=20, NA.plot = F)
Surinam <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Suriname",K=20, NA.plot = F)
turkey <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Turkije",K=20, NA.plot = F)

png("demographics.png", width = 15, height = 12, units = 'in', res = 300)
par(mfrow = c(2,5), mar=c(4, 4, 3, 1))
plot(ned$x.values, ned$f.values, type="l", xlab="% of Dutch nationals", ylab="Effect on DV")
plot(afrika$x.values, afrika$f.values, type="l", xlab="% of African nationals", ylab="Effect on DV")
plot(asia$x.values, asia$f.values, type="l", xlab="% of Asian nationals", ylab="Effect on DV")
mtext("ALE main effect of nationality on online donations", side=3, line=1.5)
plot(europe$x.values, europe$f.values, type="l", xlab="% of European nationals", ylab="Effect on DV")
plot(oceania$x.values, oceania$f.values, type="l", xlab="% of Oceania nationals", ylab="Effect on DV")
plot(Indonesia$x.values, Indonesia$f.values, type="l", xlab="% of Indonesian nationals", ylab="Effect on DV")
plot(morocco$x.values, morocco$f.values, type="l", xlab="% of Moroccan nationals", ylab="Effect on DV")
plot(NedAnt$x.values, NedAnt$f.values, type="l", xlab="% of Antilles nationals", ylab="Effect on DV")
plot(Surinam$x.values, Surinam$f.values, type="l", xlab="% of Surinam nationals", ylab="Effect on DV")
plot(turkey$x.values, turkey$f.values, type="l", xlab="% of Turkey nationals", ylab="Effect on DV")

dev.off()

#external event
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Nederland_ind",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="Europe_ind",K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J="World_ind",K=20, NA.plot = F)


#create ALE 2nd order plots for proximities
#culture
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Nederland_ind", "Nederlandse.achtergrond"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Nederland_ind", "X.voormalige..Nederlandse.Antillen..Aruba"),K=20, NA.plot = F)

ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Marokko_ind", "Marokko"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Turkije_ind", "Turkije"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Belgie_ind", "België"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Duitsland_ind", "Duitsland"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Indonesie_ind", "Indonesië"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Polen_ind", "Polen"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Suriname_ind", "Suriname"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Oceanie_ind", "Oceanië"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Afrika_ind", "Afrika"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Amerika_ind", "Amerika"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Asie_ind", "Azië"),K=20, NA.plot = F)

#location
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Nederland_ind", "Nederlandse.achtergrond"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Europe_ind", "Nederlandse.achtergrond"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("World_ind", "Nederlandse.achtergrond"),K=20, NA.plot = F)
