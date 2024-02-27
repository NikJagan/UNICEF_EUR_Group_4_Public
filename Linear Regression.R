# Step 1: import the train and test datasets
data_train <- read.csv("datasets/full_data_train.csv")
data_test <- read.csv("datasets/full_data_test.csv")


# Step 2: transformations before performing Linear Regression 
# - transform columns Nederland_ind:pledge_ind into categorical variables 
#Remove columns without observations
data_train[5:19] <- lapply(data_train[5:19], as.factor)
data_test[5:19] <- lapply(data_test[5:19], as.factor)
data_train <- data_train[, !(names(data_train) %in% c("Marokko_ind", "Belgie_ind", "Duitsland_ind", "Polen_ind"))]


# Step 3: perform the linear regression 
# - one for online donations and one for total number of donations
# - remove the irrelevant variables 
# - add your desired interaction terms

lr_online <- lm(online_donation_count ~ . - total_donations_count - Postcode - Year - Week + 
                  Nederland_ind:Nederlandse.achtergrond + Europe_ind:Nederlandse.achtergrond + 
                  World_ind:Nederlandse.achtergrond + 
                  Europe_ind:Europa..excl..Nederlandse.achtergrond. + 
                  Turkije_ind:Turkije + 
                  Indonesie_ind:Indonesië +  Suriname_ind:Suriname + 
                  Oceanie_ind:Oceanië + Afrika_ind:Afrika + Amerika_ind:Amerika + 
                  Asie_ind:Azië + Nederland_ind:X.voormalige..Nederlandse.Antillen..Aruba + 
                  pledge_ind:Nederlandse.achtergrond + pledge_ind:Afrika + 
                  pledge_ind:Amerika + pledge_ind:Azië + 
                  pledge_ind:Europa..excl..Nederlandse.achtergrond. + 
                  pledge_ind:Oceanië + pledge_ind:België + 
                  pledge_ind:Duitsland + pledge_ind:Indonesië + 
                  pledge_ind:Marokko + 
                  pledge_ind:X.voormalige..Nederlandse.Antillen..Aruba + 
                  pledge_ind:Polen + pledge_ind:Suriname + pledge_ind:Turkije
                , data = data_train)


lr_total <- lm(total_donations_count ~ . - online_donation_count - Postcode - Year - Week + 
                 Nederland_ind:Nederlandse.achtergrond + Europe_ind:Nederlandse.achtergrond + 
                 World_ind:Nederlandse.achtergrond +
                 Europe_ind:Europa..excl..Nederlandse.achtergrond. + Marokko_ind:Marokko + 
                 Turkije_ind:Turkije + Belgie_ind:België + Duitsland_ind:Duitsland + 
                 Indonesie_ind:Indonesië + Polen_ind:Polen + Suriname_ind:Suriname + 
                 Oceanie_ind:Oceanië + Afrika_ind:Afrika + Amerika_ind:Amerika + 
                 Asie_ind:Azië + Nederland_ind:X.voormalige..Nederlandse.Antillen..Aruba + 
                 pledge_ind:Nederlandse.achtergrond + pledge_ind:Afrika + 
                 pledge_ind:Amerika + pledge_ind:Azië + 
                 pledge_ind:Europa..excl..Nederlandse.achtergrond. + 
                 pledge_ind:Oceanië + pledge_ind:België + 
                 pledge_ind:Duitsland + pledge_ind:Indonesië + 
                 pledge_ind:Marokko + 
                 pledge_ind:X.voormalige..Nederlandse.Antillen..Aruba + 
                 pledge_ind:Polen + pledge_ind:Suriname + pledge_ind:Turkije
               , data = data_train)

# Step 4: obtain the output of the LR model: 
summary(lr_online)




# Compute the RMSE 
predictions_online <- predict(lr_online, newdata = data_test)
residuals_online <- data_test$online_donation_count - predictions_online 
rmse_online <- sqrt(mean(residuals_online^2))

predictions_total <- predict(lr_total, newdata = data_test)
residuals_total <- data_test$total_donations_count - predictions_total
rmse_total <- sqrt(mean(residuals_total^2))

# Write a .csv file that contains the coefficients table
summary_lr_online <- summary(lr_online)
coefftable_online <- round(summary_lr_online$coefficients, 2)
write.csv(coefftable_online, file = "coefficients_table_LR_online_donations_count.csv")

summary(lr_total)
summary_lr_total <- summary(lr_total)
coefftable_total <- round(summary_lr_total$coefficients, 2)
write.csv(coefftable_total, file = "coefficients_table_LR_total_donations_count.csv")



# Construct the feature importance plot 
# 1. Get the values of the coefficients
coefficients <- (coef(lr_online)[-1])  # Exclude the intercept term


# 2. Sort coefficients in descending order and get the order
#Only take top 3 and bottom 3 to present
order_indices <- order(coefficients)
sorted_coefficients <- coefficients[order_indices]
sorted_feature_names <- names(coefficients)[order_indices]
ind_end <- length(sorted_coefficients)
sorted_6coefficients <- sorted_coefficients[c(ind_end, ind_end-1,ind_end-2,3,2,1)]
sorted_6feature_names <- sorted_feature_names[c(ind_end, ind_end-1,ind_end-2,3,2,1)]

# 3. Create a horizontal bar plot
unicef_blue <- "#00aeef"
png("fig14_LR_feat_imp.png", width = 10, height = 8, units = 'in', res = 300)
par(mar=c(5,10,3,1))
barplot(sorted_6coefficients, col = unicef_blue,
                main = "Feature Importance in Linear Regression",
                xlab = "Coefficient Values", ylab = "", horiz = TRUE, las = 1, cex.lab=1.5, cex.main=2,
                names.arg=c("Proximity for Oceania", "Indonesian bg", "Pledge for German bg", "Polish bg", "Proximity for Dutch", "Oceania bg"))
dev.off()


# Construct the marginal effect plots using the sjPlot package
library(sjPlot)

# marginal effect plots for the pledges * migration backgrounds interaction terms
p1<-plot_model(lr_online, type = "pred", terms = c("pledge_ind", "Nederlandse.achtergrond"))
p2<-plot_model(lr_online, type = "pred", terms = c("pledge_ind", "Afrika"))
p3<-plot_model(lr_online, type = "pred", terms = c("pledge_ind", "Amerika"))
p4<-plot_model(lr_online, type = "pred", terms = c("pledge_ind", "Azië"))
p5<-plot_model(lr_online, type = "pred", terms = c("pledge_ind", "Europa..excl..Nederlandse.achtergrond."))
p6<-plot_model(lr_online, type = "pred", terms = c("pledge_ind", "Oceanië"))
p7<-plot_model(lr_online, type = "pred", terms = c("pledge_ind", "Indonesië"))
p8<-plot_model(lr_online, type = "pred", terms = c("pledge_ind", "X.voormalige..Nederlandse.Antillen..Aruba"))
p9<-plot_model(lr_online, type = "pred", terms = c("pledge_ind", "Suriname"))
p10<-plot_model(lr_online, type = "pred", terms = c("pledge_ind", "Turkije"))
g<- grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, nrow=5, top = "Marginal effect plots for the pledges * migration backgrounds interaction terms")
ggsave("fig15_marginal_effects_1.png", g)


# marginal effect plots for the local proximity captured by the Dutch background * local vs European vs. rest of the world events
p1<-plot_model(lr_online, type = "pred", terms = c("Nederland_ind", "Nederlandse.achtergrond"))
p2<-plot_model(lr_online, type = "pred", terms = c("Europe_ind", "Nederlandse.achtergrond"))
p3<-plot_model(lr_online, type = "pred", terms = c("World_ind", "Nederlandse.achtergrond"))
g<- grid.arrange(p1,p2,p3, nrow=1, top = "Marginal effect plots for the location proximity")
ggsave("fig15_marginal_effects_2.png", g)

# marginal effect plots for the cultural proximity captured by the migration background * event location interaction
p1<-plot_model(lr_online, type = "pred", terms = c("Marokko_ind", "Marokko"))
p2<-plot_model(lr_online, type = "pred", terms = c("Turkije_ind", "Turkije"))
p3<-plot_model(lr_online, type = "pred", terms = c("Indonesie_ind", "Indonesië"))
p4<-plot_model(lr_online, type = "pred", terms = c("Suriname_ind", "Suriname"))
p5<-plot_model(lr_online, type = "pred", terms = c("Oceanie_ind", "Oceanië"))
p6<-plot_model(lr_online, type = "pred", terms = c("Afrika_ind", "Afrika"))
p7<-plot_model(lr_online, type = "pred", terms = c("Amerika_ind", "Amerika"))
p8<-plot_model(lr_online, type = "pred", terms = c("Asie_ind", "Azië"))
p9<-plot_model(lr_online, type = "pred", terms = c("Nederland_ind", "X.voormalige..Nederlandse.Antillen..Aruba"))
g<- grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9 nrow=3, top = "Marginal effect plots for the cultural proximity")
ggsave("fig15_marginal_effects_3.png", g)




