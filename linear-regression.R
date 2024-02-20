#Linear regression


# Step 1: import the data
data <- read.csv("cleaned_joined_data.csv")

# Step 2: transformations before performing Linear Regression 
# - transform columns Nederland_ind:Asie_ind into categorical variables 
data[6:19] <- lapply(data[6:19], as.factor)



# Step 3: Perform the linear regression as specified in equation 1 in our paper: 
# online donations on pledges, event and characteristics 
# + interaction of events and characteristics + interaction of pledges and characteristics 
# explicity include interaction terms using x1:x2 for the interaction between x1 and x2 
lr_model <- lm(donation_count ~ . - Postcode - Year - Week + Nederland_ind:Nederlandse.achtergrond + 
                 Europe_ind:Europa..excl..Nederlandse.achtergrond. + Marokko_ind:Marokko + 
                 Turkije_ind:Turkije + Belgie_ind:België + Duitsland_ind:Duitsland + 
                 Indonesie_ind:Indonesië + Polen_ind:Polen + Suriname_ind:Suriname + 
                 Oceanie_ind:Oceanië + Afrika_ind:Afrika + Amerika_ind:Amerika + 
                 Asie_ind:Azië + Nederland_ind:X.voormalige..Nederlandse.Antillen..Aruba + 
                 pledge_count:Nederlandse.achtergrond + pledge_count:Afrika + 
                 pledge_count:Amerika + pledge_count:Azië + 
                 pledge_count:Europa..excl..Nederlandse.achtergrond. + 
                 pledge_count:Oceanië + pledge_count:België + 
                 pledge_count:Duitsland + pledge_count:Indonesië + 
                 pledge_count:Marokko + 
                 pledge_count:X.voormalige..Nederlandse.Antillen..Aruba + 
                 pledge_count:Polen + pledge_count:Suriname + pledge_count:Turkije
               , data = data)

# Step 4: Output of the Linear regression Model
summary(lr_model)

# Q: Are we going to use 'bevolking' (the last column)? 
# Standardize using 'bevolking'? Or by number of households?? 