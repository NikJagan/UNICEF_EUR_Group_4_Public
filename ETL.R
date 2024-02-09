# Load necessary libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(data.table)

# Read the files
online_gifts <- fread("datasets/ONLINE GIFTS.csv", sep = ";")
pledges <- fread("datasets/PLEDGES.csv", sep = ";")
external_events <- fread("datasets/external_events_data.csv", , sep = ",")
migration_data <- fread("datasets/Population_migrationbackground_postcode.csv", sep = ";")

# Removing rows with empty entries and NAs
online_gifts[online_gifts == ""] <- NA
pledges[pledges == ""] <- NA
migration_data[migration_data == ""] <- NA
online_gifts <- na.omit(online_gifts)
pledges <- na.omit(pledges)
zipcodes <- na.omit(zipcodes)
migration_data <- na.omit(migration_data)

# Trimming whitespace of PC6 and removing empty PC6
online_gifts$PC6 <- trimws(online_gifts$PC6)
pledges$PC6 <- trimws(pledges$PC6)
online_gifts <- online_gifts[online_gifts$PC6 != "", ]
pledges <- pledges[pledges$PC6 != "", ]

# Remove non-numeric values in Postcodes
migration_data <- migration_data[!grepl("\\D", migration_data$Postcode), ]

#bring dates to correct format
external_events$datum <- ymd(external_events$datum) 
online_gifts$DATE <- dmy(format(mdy(online_gifts$DATE), "%d/%m/%Y"))
pledges$ACQUISITIONDATE <- dmy(format(mdy(pledges$ACQUISITIONDATE), "%d.%m.%Y"))

# Join the pledges and online gifts datasets on PC6 + dates
# we only consider online donations made between pledge date 14 days in the future
# as "eligible" to identify caryover effect
pledges$ACQUISITIONDATE_end_cooloff <- pledges$ACQUISITIONDATE+14

pledges_plus_online_gifts <- full_join(pledges, online_gifts, join_by(PC6==PC6, ACQUISITIONDATE <= DATE, ACQUISITIONDATE_end_cooloff >= DATE ))

# take care of NAs
# If gift ID or pledge id, it means this type of donation was unavailable
pledges_plus_online_gifts[,c("PLEDGE_ID", "GIFT_ID")][!is.na(pledges_plus_online_gifts[,c("PLEDGE_ID", "GIFT_ID")])] <- 1
pledges_plus_online_gifts[,c("PLEDGE_ID", "GIFT_ID")][is.na(pledges_plus_online_gifts[,c("PLEDGE_ID", "GIFT_ID")])] <- 0

# Rename date columns for consistency
pledges_plus_online_gifts <- setnames(pledges_plus_online_gifts, old = "DATE", new = "ONLINE_GIFT_DATE")
pledges_plus_online_gifts <-setnames(pledges_plus_online_gifts,  old = "ACQUISITIONDATE", new = "PLEDGE_DATE")

# if pledge date is available, use it, otherwise online donation
pledges_plus_online_gifts$DATE_external_event_end <- dplyr::coalesce(pledges_plus_online_gifts$PLEDGE_DATE, 
                                      pledges_plus_online_gifts$ONLINE_GIFT_DATE, NA)
pledges_plus_online_gifts$DATE_external_event_beg <- pledges_plus_online_gifts$DATE_external_event_end-7

#join the external events dataset
ind_cols = c("Nederland_ind","Europe_ind","World_ind","Marokko_ind","Turkije_ind","Belgie_ind","Duitsland_ind","Indonesie_ind","Polen_ind", "Suriname_ind","Oceanie_ind","Afrika_ind","Amerika_ind","Asie_ind")
joined_data <- left_join(pledges_plus_online_gifts,external_events, join_by(DATE_external_event_end>=datum, DATE_external_event_beg<=datum))
joined_data <- joined_data[ , (ind_cols) := lapply(.SD, nafill, fill=0), .SDcols = ind_cols]

# Convert Pc6 to PC4
joined_data$PC6 <- gsub("[[:alpha:]]", "", joined_data$PC6)

# Rename PC6 to Postcode for consistency
colnames(joined_data)[colnames(joined_data) == "PC6"] <- "Postcode"

# Add a week column to each dataset
joined_data$Week <- isoweek(joined_data$DATE_external_event_end)

# Add a year column to each dataset and only take data from 2018 (no external events for 2018)
joined_data$Year <- year(joined_data$DATE_external_event_end)
joined_data <- joined_data[Year>2018,]

write.csv(joined_data, "datasets/not_aggregated_data_description.csv", row.names=F)


#aggregate data on the correct level
joined_data <- joined_data %>% group_by(Postcode,Year, Week) %>%
                summarise(pledge_count=sum(PLEDGE_ID),
                          donation_count=sum(GIFT_ID),
                          Nederland_ind=max(Nederland_ind),
                          Europe_ind=max(Europe_ind),
                          World_ind=max(World_ind),
                          Marokko_ind=max(Marokko_ind),
                          Turkije_ind=max(Turkije_ind),
                          Belgie_ind=max(Belgie_ind),
                          Duitsland_ind=max(Duitsland_ind),
                          Indonesie_ind=max(Indonesie_ind),
                          Polen_ind=max(Polen_ind),
                          Suriname_ind=max(Suriname_ind),
                          Oceanie_ind=max(Oceanie_ind),
                          Afrika_ind=max(Afrika_ind),
                          Amerika_ind=max(Amerika_ind),
                          Asie_ind=max(Asie_ind),
                )

#MIGRATION DATA ####

#Delete Gender Column (it is just 1 variable all the time)
migration_data$Geslacht <- NULL

#Change Column names for consistency
setnames(migration_data, "Bevolking (aantal)", "Bevolking")
setnames(migration_data, "Perioden", "YEAR")
setnames(migration_data, "Postcode", "PC4")

#Remove irrelevant Rows
migration_data <- migration_data[PC4 != "Niet in te delen" & PC4 != "Nederland"]
migration_data <- migration_data %>% mutate_if(is.character, ~na_if(., ""))
migration_data <- migration_data[complete.cases(migration_data), ] #remove missing values

#Transform from char into Integer
migration_data$Bevolking <- as.integer(migration_data$Bevolking)

#Add a column that shows the percentage of a group (nationality etc.) compared to the total population in a given zipcode
migration_dat <- migration_data %>%
	group_by(YEAR, PC4) %>%
	mutate(Percentage = (Bevolking / sum(Bevolking[Migratieachtergrond == "Totaal"])))
migration_dat <- migration_dat %>% mutate(Percentage = round(Percentage, 2))
migration_dat <- na.omit(migration_dat)

#Transform Rows into Columns
migration_dat <- migration_dat %>%
	pivot_wider(names_from = Migratieachtergrond, values_from = Percentage, values_fill = 0) %>%
	group_by(PC4, YEAR) %>%
	summarize(across(c(Totaal:`Overige westerse migratieachtergrond`), sum, na.rm = TRUE))

migration_dat <- migration_dat[migration_dat$Totaal != 0, ]

#Do we need to adjust for population sizes?
# We do have some Outliers. We could inlcude the absolute sizes of every postcode. 

#Add absolute Poplulation Size
total <-  migration_data[Migratieachtergrond == "Totaal", ]
total[, "Migratieachtergrond"] <- NULL
migration <- left_join(migration_dat, total, by = c("PC4", "YEAR"))

#Delete Totaal (is constant)
migration[, "Totaal"] <- NULL

#scale Bevolking column
migration$Bevolking <- scale(migration$Bevolking)

#Delete redundant columns according to our external events proximities
migration[, "Met migratieachtergrond"] <- NULL
migration[, "Europese Unie (excl. Nederlandse acht..."] <- NULL
migration[, "Niet-westerse migratieachtergrond"] <- NULL
migration[, "Westerse migratieachtergrond"] <- NULL
migration[, "Overige westerse migratieachtergrond"] <- NULL
migration[, "Overige niet-westerse migratieachterg..."] <- NULL
migration <- setnames(migration, old = "YEAR", new = "Year")
setnames(migration, old = "PC4", new = "Postcode")

#Final Data
full_data <- left_join(joined_data, migration, by = c("Year", "Postcode"))
full_data <- na.omit(full_data)

write.csv(full_data, "data/cleaned_joined_data.csv ", row.names=F)
