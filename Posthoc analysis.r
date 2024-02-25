library(ALEPlot)
library(randomForest)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(grid)

############################
#########ALE################
############################

#read the model 
load("RandomForest.RData")
rf_model <- tuned_model

#define the prediction func
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))

#define design constants
colour_palette <- c("#caf0f8","#ade8f4", "#90e0ef", "#48cae4", "#00b4d8", "#0096c7", "#0077b6", "#023e8a")
bg_col<-"#EFEEE7"
unicef_blue <- "#00aeef"

#create ALE 1st order plots for most important vars
#pledges
ALE_pledge <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="pledge_ind",K=20, NA.plot = F)
ALE_pledge <- data.frame(x=ALE_pledge$x.values, y=ALE_pledge$f.values)
ggplot(data=ALE_pledge, aes(x=x, y=y)) +
  geom_bar(stat="identity", fill=unicef_blue) +
  geom_text(aes(label=round(y, 2)), vjust=c(2, -1),color="white", size=3.6)+ 
  labs(x="Presence of a pledge", y="Effect on DV", title="ALE main effect of pledges on online donations")+
  theme_classic() 
  #theme(panel.background = element_rect(fill=bg_col), plot.background = element_rect(fill=bg_col))
ggsave('fig8_pledges_effect.png')
#ggsave('prezzo_pledges_effect.png')


#demographics
ned <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Nederlandse.achtergrond",K=20, NA.plot = T)
afrika <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Afrika",K=20, NA.plot = F)
asia <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Azië",K=20, NA.plot = F)
europe <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Europa..excl..Nederlandse.achtergrond.",K=20, NA.plot = F)
amerika <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Amerika",K=20, NA.plot = F)
Indonesia <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Indonesië",K=20, NA.plot = F)
NedAnt <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="X.voormalige..Nederlandse.Antillen..Aruba",K=20, NA.plot = F)
Surinam <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Suriname",K=20, NA.plot = F)
turkey <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Turkije",K=20, NA.plot = F)

png("fig9_demographics_effect.png", width = 10, height = 8, units = 'in', res = 300)
par(mfrow = c(3,3), mar=c(4, 4, 3, 1), oma=c(2,2,3,2))
#nationalities close to Dutch
plot(ned$x.values, ned$f.values, type="l", xlab="% of Dutch nationals", ylab="", ylim=c(-0.05, 0.2))
#plot(NedAnt$x.values, NedAnt$f.values, type="l", xlab="% of Antilles nationals", ylab="", ylim=c(-0.05, 0.2))
#plot(europe$x.values, europe$f.values, type="l", xlab="% of European nationals", ylab="", ylim=c(-0.05, 0.2))
mtext("ALE main effect of nationality on online donations", side=3, outer=TRUE)
#nationalities with most external events
plot(afrika$x.values, afrika$f.values, type="l", xlab="% of African nationals", ylab="", ylim=c(-0.05, 0.2))
plot(asia$x.values, asia$f.values, type="l", xlab="% of Asian nationals", ylab="", ylim=c(-0.05, 0.2))
plot(amerika$x.values, amerika$f.values, type="l", xlab="% of American nationals", ylab="", ylim=c(-0.05, 0.2))
#nationalities with history with Dutch
plot(Indonesia$x.values, Indonesia$f.values, type="l", xlab="% of Indonesian nationals", ylab="", ylim=c(-0.05, 0.2))
plot(Surinam$x.values, Surinam$f.values, type="l", xlab="% of Surinam nationals", ylab="", ylim=c(-0.05, 0.2))
plot(turkey$x.values, turkey$f.values, type="l", xlab="% of Turkey nationals", ylab="", ylim=c(-0.05, 0.2))
mtext("Increase in online donations", side=2, line=0.6, cex=0.9, outer=TRUE)
dev.off()

#plot for presenting with most important demographics according to feature importance
png("prezzo_demographics_effect.png", width = 10, height = 8, units = 'in', res = 300)
par(mfrow = c(2,2), mar=c(4, 4, 3, 1), oma=c(2,2,3,2), bg=bg_col)
plot(ned$x.values, ned$f.values, type="l", xlab="% of Dutch nationals", ylab="", ylim=c(-0.05, 0.2))
mtext("ALE main effect of nationality on online donations", side=3, outer=TRUE)
plot(afrika$x.values, afrika$f.values, type="l", xlab="% of African nationals", ylab="", ylim=c(-0.05, 0.2))
plot(asia$x.values, asia$f.values, type="l", xlab="% of Asian nationals", ylab="", ylim=c(-0.05, 0.2))
plot(amerika$x.values, amerika$f.values, type="l", xlab="% of American nationals", ylab="", ylim=c(-0.05, 0.2))
mtext("Increase in online donations", side=2, line=0.6, cex=0.9, outer=TRUE)
dev.off()

#external events
ned_ev <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Nederland_ind",K=20, NA.plot = F)
ned_ev <- data.frame(x=ned_ev$x.values, y=ned_ev$f.values)
eu_ev <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="Europe_ind",K=20, NA.plot = F)
eu_ev <- data.frame(x=eu_ev$x.values, y=eu_ev$f.values)
world_ev <- ALEPlot(df_train, rf_model, pred.fun=yhat, J="World_ind",K=20, NA.plot = F)
world_ev <- data.frame(x=world_ev$x.values, y=world_ev$f.values)

p1<-ggplot(data=ned_ev, aes(x=x, y=y)) +
  geom_bar(stat="identity", fill=unicef_blue) +
  ylim(c(-0.2,0.25)) +
  geom_text(aes(label=round(y, 2)), vjust=c(-0.1, 1),color="white", size=2.5)+ 
  labs(x="Event in NL", y="", title="")+
  theme_classic() #+ theme(panel.background = element_rect(fill=bg_col), rect = element_rect(fill = 'bg_col'), plot.background = element_rect(fill=bg_col))

p2<-ggplot(data=eu_ev, aes(x=x, y=y)) +
  geom_bar(stat="identity", fill=unicef_blue) +
  ylim(c(-0.2,0.25)) +
  geom_text(aes(label=round(y, 2)), vjust=c(-0.5, 1),color="white", size=2.5)+ 
  labs(x="Event in EU", y="", title="")+
  theme_classic() #+ theme(panel.background = element_rect(fill=bg_col),rect = element_rect(fill = 'bg_col'),  plot.background = element_rect(fill=bg_col))

p3<-ggplot(data=world_ev, aes(x=x, y=y)) +
  geom_bar(stat="identity", fill=unicef_blue) +
  ylim(c(-0.2,0.25)) +
  geom_text(aes(label=round(y, 2)), vjust=c(-0.5, 1),color="white", size=2.5)+ 
  labs(x="Event elsewhere", y="", title="")+
  theme_classic() #+ theme(panel.background = element_rect(fill=bg_col),rect = element_rect(fill = 'bg_col'), plot.background = element_rect(fill=bg_col))

g<- grid.arrange(p1,p2,p3, nrow=1, top = "ALE main effect of external events on online donations", left="Increase in online donations")
#g<- grid.draw(grobTree(rectGrob(gp=gpar(fill=bg_col, lwd=0)), g))
ggsave("fig10_events_effect.png", g)

#create ALE 2nd order plots for proximities
#culture
#plot 3 biggest nationality groups
nl <- ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Nederland_ind", "Nederlandse.achtergrond"),K=20, NA.plot = F)
vorm_nl <- ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Nederland_ind", "X.voormalige..Nederlandse.Antillen..Aruba"),K=20, NA.plot = F)
EU <- ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Europe_ind", "Europa..excl..Nederlandse.achtergrond."),K=20, NA.plot = F)

#plot 3 nationalities historically connected with NL
ind <- ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Indonesie_ind", "Indonesië"),K=20, NA.plot = F)
surinam <- ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Suriname_ind", "Suriname"),K=20, NA.plot = F)
turkey <- ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Turkije_ind", "Turkije"),K=20, NA.plot = F)

#plot 3 nationalities with most external events
africa <- ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Afrika_ind", "Afrika"),K=20, NA.plot = F)
america <- ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Amerika_ind", "Amerika"),K=20, NA.plot = F)
asia <- ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Asie_ind", "Azië"),K=20, NA.plot = F)

png("fig11_cultural_interaction.png", width = 6, height = 10, units = 'in', res = 500)
par(mfrow = c(3,3), mar=c(4, 4, 3, 1), oma=c(3,3,3,1))
image(as.integer(nl$x.values[[1]]), nl$x.values[[2]], nl$f.values,xlab = "the Netherlands", ylab = "Dutch", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
mtext("Darker colours indicate higher online donation count", side=3, adj=-1.5,line=1, cex=0.7)
image(as.integer(vorm_nl$x.values[[1]]), vorm_nl$x.values[[2]], vorm_nl$f.values, xlab = "the Netherlands", ylab = "the Netherlands Antilles", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(EU$x.values[[1]]), EU$x.values[[2]], EU$f.values, xlab = "Europe (excl.NL)", ylab = "European", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
mtext("ALE effect plot for cultural proximities", side=3, line=1.5, cex=0.9, outer=TRUE)
image(as.integer(ind$x.values[[1]]), ind$x.values[[2]], ind$f.values, xlab = "Indonesia", ylab = "Indonesian", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(surinam$x.values[[1]]), surinam$x.values[[2]], surinam$f.values, xlab = "Surinam", ylab = "Surinamese", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(turkey$x.values[[1]]), turkey$x.values[[2]], turkey$f.values, xlab = "Turkey", ylab = "Turkish", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(africa$x.values[[1]]), africa$x.values[[2]], africa$f.values, xlab = "Africa", ylab = "African", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(america$x.values[[1]]), america$x.values[[2]], america$f.values, xlab = "America", ylab = "American", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(asia$x.values[[1]]), asia$x.values[[2]], asia$f.values, xlab = "Asia", ylab = "Asian", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
mtext("Place where event happened (1=event happened)", side=1, line=1.5, cex=0.9, outer=TRUE)
mtext("% of specific cultural background in zipcode", side=2, line=1.5, cex=0.9, outer=TRUE)
dev.off()

#plots for presentation
png("prezzo_cultural_interaction.png", width = 6, height = 10, units = 'in', res = 500)
par(mfrow = c(2,2), mar=c(4, 4, 3, 1), oma=c(3,3,3,1), bg=bg_col)
image(as.integer(nl$x.values[[1]]), nl$x.values[[2]], nl$f.values,xlab = "the Netherlands", ylab = "Dutch", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
mtext("Darker colours indicate higher online donation count", side=3, adj=-1.5,line=1, cex=0.7)
mtext("ALE effect plot for cultural proximities", side=3, line=1.5, cex=0.9, outer=TRUE)
image(as.integer(africa$x.values[[1]]), africa$x.values[[2]], africa$f.values, xlab = "Africa", ylab = "African", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(america$x.values[[1]]), america$x.values[[2]], america$f.values, xlab = "America", ylab = "American", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(asia$x.values[[1]]), asia$x.values[[2]], asia$f.values, xlab = "Asia", ylab = "Asian", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
mtext("Place where event happened (1=event happened)", side=1, line=1.5, cex=0.9, outer=TRUE)
mtext("% of specific cultural background in zipcode", side=2, line=1.5, cex=0.9, outer=TRUE)
dev.off()

#location
nl_loc <- ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Nederland_ind", "Nederlandse.achtergrond"),K=20, NA.plot = F)
eu_loc <- ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("Europe_ind", "Nederlandse.achtergrond"),K=20, NA.plot = F)
world_loc <- ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("World_ind", "Nederlandse.achtergrond"),K=20, NA.plot = F)

png("fig12_location_interaction.png", width = 6, height = 10, units = 'in', res = 500)
par(mfrow = c(1,3), mar=c(4, 3, 3, 1), oma=c(2,4,3,1))
image(as.integer(nl_loc$x.values[[1]]), nl_loc$x.values[[2]], nl_loc$f.values,xlab = "Event in NL", ylab = "", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
mtext("Darker colours indicate higher online donation count", side=3, adj=-1.5,line=1, cex=0.7)
image(as.integer(eu_loc$x.values[[1]]), eu_loc$x.values[[2]], eu_loc$f.values, xlab = "Event in EU", ylab = "", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(world_loc$x.values[[1]]), world_loc$x.values[[2]], world_loc$f.values, xlab = "Event elsewhere", ylab = "", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
mtext("Darker colours indicate higher online donation count", side=3, adj=-2,line=1, cex=0.7)
mtext("ALE effect plot for location proximities", side=3, line=1.5, cex=0.9, outer=TRUE)
dev.off()

#pledge interaction
nl_dm <- ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Nederlandse.achtergrond"),K=20, NA.plot = F)
vorm_nl_dm <-ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "X.voormalige..Nederlandse.Antillen..Aruba"),K=20, NA.plot = F)
tk_dm <-ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Turkije"),K=20, NA.plot = F)
ind_dm <-ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Indonesië"),K=20, NA.plot = F)
sur_dm <-ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Suriname"),K=20, NA.plot = F)
afr_dm <-ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Afrika"),K=20, NA.plot = F)
am_dm <-ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Amerika"),K=20, NA.plot = F)
as_dm <-ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Azië"),K=20, NA.plot = F)
eu_dm <-ALEPlot(df_train, rf_model, pred.fun=yhat, J=c("pledge_ind", "Europa..excl..Nederlandse.achtergrond."),K=20, NA.plot = F)

png("fig13_pledge-demographic_interaction.png", width = 6, height = 10, units = 'in', res = 500)
par(mfrow = c(3,3), mar=c(4, 4, 3, 1), oma=c(3,3,3,1))
image(as.integer(nl_dm$x.values[[1]]), nl_dm$x.values[[2]], nl_dm$f.values,xlab = "", ylab = "Dutch", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
mtext("Darker colours indicate higher online donation count", side=3, adj=-1.5,line=1, cex=0.7)
image(as.integer(vorm_nl_dm$x.values[[1]]), vorm_nl_dm$x.values[[2]], vorm_nl_dm$f.values, xlab = "", ylab = "the Netherlands Antilles", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(eu_dm$x.values[[1]]), eu_dm$x.values[[2]], eu_dm$f.values, xlab = "", ylab = "European", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(tk_dm$x.values[[1]]), tk_dm$x.values[[2]], tk_dm$f.values, xlab = "", ylab = "Turkish", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
mtext("ALE effect plot for demographics * pledges interaction", side=3, line=1.5, cex=0.9, outer=TRUE)
image(as.integer(ind_dm$x.values[[1]]), ind_dm$x.values[[2]], ind_dm$f.values, xlab = "", ylab = "Indonesian", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(sur_dm$x.values[[1]]), sur_dm$x.values[[2]], sur_dm$f.values, xlab = "", ylab = "Surinamese", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(afr_dm$x.values[[1]]), afr_dm$x.values[[2]], afr_dm$f.values, xlab = "", ylab = "African", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(am_dm$x.values[[1]]), am_dm$x.values[[2]], am_dm$f.values, xlab = "", ylab = "American", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(as_dm$x.values[[1]]), as_dm$x.values[[2]], as_dm$f.values, xlab = "", ylab = "Asian", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
mtext("Presence of pledge in zip code (1=pledge happened)", side=1, line=1.5, cex=0.9, outer=TRUE)
mtext("% of specific cultural background in zipcode", side=2, line=1.5, cex=0.9, outer=TRUE)
dev.off()

#plots for presentation
png("prezzo_pledge-demographic_interaction.png", width = 6, height = 10, units = 'in', res = 500)
par(mfrow = c(2,2), mar=c(4, 4, 3, 1), oma=c(3,3,3,1), bg=bg_col)
image(as.integer(nl_dm$x.values[[1]]), nl_dm$x.values[[2]], nl_dm$f.values,xlab = "", ylab = "Dutch", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
mtext("Darker colours indicate higher online donation count", side=3, adj=-1.5,line=1, cex=0.7)
image(as.integer(afr_dm$x.values[[1]]), afr_dm$x.values[[2]], afr_dm$f.values, xlab = "", ylab = "African", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(am_dm$x.values[[1]]), am_dm$x.values[[2]], am_dm$f.values, xlab = "", ylab = "American", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
image(as.integer(as_dm$x.values[[1]]), as_dm$x.values[[2]], as_dm$f.values, xlab = "", ylab = "Asian", col=colour_palette, xaxt="n")
axis(1, at=c(0,1),labels=c("0","1"))
mtext("Presence of pledge in zip code (1=pledge happened)", side=1, line=1.5, cex=0.9, outer=TRUE)
mtext("% of specific cultural background in zipcode", side=2, line=1.5, cex=0.9, outer=TRUE)
dev.off()

################################
#########SHAPLEY################
################################
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
