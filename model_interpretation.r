library(ALEPlot)
library(randomForest)
library(ale)
library(dplyr)

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
NedAnt <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="X.voormalige..Nederlandse.Antillen..Aruba",K=20, NA.plot = F)
Surinam <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Suriname",K=20, NA.plot = F)
turkey <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Turkije",K=20, NA.plot = F)

png("demographics.png", width = 10, height = 8, units = 'in', res = 300)
par(mfrow = c(3,3), mar=c(4, 4, 3, 1))
plot(ned$x.values, ned$f.values, type="l", xlab="% of Dutch nationals", ylab="Effect on DV")
plot(afrika$x.values, afrika$f.values, type="l", xlab="% of African nationals", ylab="Effect on DV")
plot(asia$x.values, asia$f.values, type="l", xlab="% of Asian nationals", ylab="Effect on DV")
mtext("ALE main effect of nationality on online donations", side=3, line=1.5)
plot(europe$x.values, europe$f.values, type="l", xlab="% of European nationals", ylab="Effect on DV")
plot(oceania$x.values, oceania$f.values, type="l", xlab="% of Oceania nationals", ylab="Effect on DV")
plot(Indonesia$x.values, Indonesia$f.values, type="l", xlab="% of Indonesian nationals", ylab="Effect on DV")
plot(NedAnt$x.values, NedAnt$f.values, type="l", xlab="% of Antilles nationals", ylab="Effect on DV")
plot(Surinam$x.values, Surinam$f.values, type="l", xlab="% of Surinam nationals", ylab="Effect on DV")
plot(turkey$x.values, turkey$f.values, type="l", xlab="% of Turkey nationals", ylab="Effect on DV")
dev.off()

#external event
ned_ev <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Nederland_ind",K=20, NA.plot = F)
eu_ev <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Europe_ind",K=20, NA.plot = F)
world_ev <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="World_ind",K=20, NA.plot = F)

png("events.png", width = 10, height = 8, units = 'in', res = 300)
par(mfrow = c(1,3), mar=c(4, 4, 3, 1))
plot(ned_ev$x.values, ned_ev$f.values, type="l", xlab="A disaster happened in the Netherlands", ylab="Effect on DV")
plot(eu_ev$x.values, eu_ev$f.values, type="l", xlab="A disaster happened in EU", ylab="Effect on DV")
mtext("ALE main effect of external events on online donations", side=3, line=1.5)
plot(world_ev$x.values, world_ev$f.values, type="l", xlab="A disaster happened elsewehere", ylab="Effect on DV")
dev.off()

#create ALE 2nd order plots for proximities
#culture
png("culture_interaction.png", width = 6, height = 10, units = 'in', res = 500)
par(mfrow = c(3,3), mar=c(4, 4, 3, 1), oma=c(3,3,3,1))
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Nederland_ind", "Nederlandse.achtergrond"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Nederland_ind", "X.voormalige..Nederlandse.Antillen..Aruba"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Turkije_ind", "Turkije"),K=20, NA.plot = F)
mtext("ALE effect plot for cultural proximities", side=3, line=1.5, cex=0.7, outer=TRUE)
mtext("% of this nationality in the zip code", side=2, line=1.5, cex=0.7, outer=TRUE)
mtext("Region where an event happened (1=happened)", side=1, line=1.5, cex=0.7, outer=TRUE)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Indonesie_ind", "Indonesië"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Suriname_ind", "Suriname"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Afrika_ind", "Afrika"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Amerika_ind", "Amerika"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Asie_ind", "Azië"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Oceanie_ind", "Oceanië"),K=20, NA.plot = F)
dev.off()

#location
png("loc_interaction.png", width = 6, height = 10, units = 'in', res = 500)
par(mfrow = c(1,3), mar=c(3, 3, 3, 1), oma=c(4,4,3,1))
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Nederland_ind", "Nederlandse.achtergrond"),K=20, NA.plot = F)
mtext("Event happened in NL", side=1, line=2, cex=0.7, outer=TRUE, adj=0)
mtext("% of Dutch nationals", side=2, line=2, cex=0.7, outer=TRUE)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Europe_ind", "Nederlandse.achtergrond"),K=20, NA.plot = F)
mtext("Event happened in EU (excl NL)", side=1, line=2, cex=0.7, outer=TRUE)
mtext("ALE main effect of external events on online donations", side=3, line=1.5, outer=TRUE)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("World_ind", "Nederlandse.achtergrond"),K=20, NA.plot = F)
mtext("Event happened elsewhere", side=1, line=2, cex=0.7, outer=TRUE, adj=1)
dev.off()

#pledge interaction
png("pledge_interaction.png", width = 6, height = 10, units = 'in', res = 500)
par(mfrow = c(3,3), mar=c(4, 4, 3, 1), oma=c(3,3,3,1))
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Nederlandse.achtergrond"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "X.voormalige..Nederlandse.Antillen..Aruba"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Turkije"),K=20, NA.plot = F)
mtext("ALE effect plot for pledge x demographics interactions", side=3, line=1.5, cex=0.7, outer=TRUE)
mtext("% of this nationality in the zip code", side=2, line=1.5, cex=0.7, outer=TRUE)
mtext("Presence of pledge (1=happened)", side=1, line=1.5, cex=0.7, outer=TRUE)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Indonesië"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Suriname"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Afrika"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Amerika"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Azië"),K=20, NA.plot = F)
ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Oceanië"),K=20, NA.plot = F)
dev.off()


#ALE package
df_train_ale <- as.data.frame(df_train)
ale_model <- ale_ixn(df_test, rf_model, y_col="online_donation_count")

help(ale)
help(predict)
str(df_train)
