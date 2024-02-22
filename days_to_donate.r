
library(data.table)
library(tidyverse)
library(lubridate)

online_gifts <- read.csv("unicef_donations_data/ONLINE GIFTS.csv", sep = ";")
pledges <- read.csv("unicef_donations_data/PLEDGES.csv", sep = ";")
zipcodes <- read.csv("unicef_donations_data/ZIPCODES.csv", sep = ";")
external_events <- read.csv("external_events_data.csv")
migration_data <- read.csv("demographics_data/Population_migrationbackground_postcode.csv", sep = ";")
additional_info <- read.csv2("extended-data/ADDITIONAL INFO 2VARS PC6.csv", sep = ";")

# Removing rows with empty entries
external_events[external_events == ""] <- NA
online_gifts[online_gifts == ""] <- NA
pledges[pledges == ""] <- NA
zipcodes[zipcodes == ""] <- NA
migration_data[migration_data == ""] <- NA
additional_info[additional_info == ""] <- NA

online_gifts <- na.omit(online_gifts)
pledges <- na.omit(pledges)
zipcodes <- na.omit(zipcodes)
external_events <- na.omit(external_events)
migration_data <- na.omit(migration_data)
additional_info <- na.omit(additional_info)

# Trimming whitespace of PC6 and removing empty PC6
online_gifts$PC6 <- trimws(online_gifts$PC6)
pledges$PC6 <- trimws(pledges$PC6)
zipcodes$PC6 <- trimws(zipcodes$PC6)
additional_info$PC <- trimws(additional_info$PC)

online_gifts <- online_gifts[online_gifts$PC6 != "", ]
pledges <- pledges[pledges$PC6 != "", ]
zipcodes <- zipcodes[zipcodes$PC6 != "", ]
additional_info <- additional_info[additional_info$PC != "", ]

external_events$datum <- dmy(external_events$datum)
online_gifts$DATE <- dmy(format(mdy(online_gifts$DATE), "%d/%m/%Y"))
pledges$ACQUISITIONDATE <- dmy(format(mdy(pledges$ACQUISITIONDATE), "%d.%m.%Y"))

joined_data<- left_join(pledges, online_gifts, join_by(PC6==PC6,ACQUISITIONDATE <= DATE))
joined_data <- na.omit(joined_data)

joined_data$days_diff <- as.numeric(difftime(joined_data$DATE,joined_data$ACQUISITIONDATE, units="days"))
ggplot(joined_data, aes(x=days_diff)) + 
geom_histogram(fill="#1CABE2") + xlim(c(0,30)) +
theme_classic() + 
ylim(0,100) +
geom_vline(xintercept = 14, linetype="dotted", color = "blue", size=1.5) +
labs(x="Difference in days between pledge date and online donation date", y="Frequency") 
ggsave("fig2_days_donations_distr.png")
