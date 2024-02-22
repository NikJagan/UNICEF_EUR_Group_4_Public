
library(data.table)
library(tidyverse)
library(lubridate)

online_gifts <- fread("datasets/ONLINE GIFTS.csv", sep = ";")
pledges <- fread("datasets/PLEDGES.csv", sep = ";")
zipcodes <- fread("datasets/ZIPCODES.csv", sep = ";")
external_events <- fread("datasets/external_events_data.csv", , sep = ",")
migration_data <- fread("datasets/Population_migrationbackground_postcode.csv", sep = ";")

# Removing rows with empty entries
external_events[external_events == ""] <- NA
online_gifts[online_gifts == ""] <- NA
pledges[pledges == ""] <- NA
zipcodes[zipcodes == ""] <- NA


online_gifts <- na.omit(online_gifts)
pledges <- na.omit(pledges)
zipcodes <- na.omit(zipcodes)
external_events <- na.omit(external_events)

# Trimming whitespace of PC6 and removing empty PC6
online_gifts$PC6 <- trimws(online_gifts$PC6)
pledges$PC6 <- trimws(pledges$PC6)
zipcodes$PC6 <- trimws(zipcodes$PC6)


online_gifts <- online_gifts[online_gifts$PC6 != "", ]
pledges <- pledges[pledges$PC6 != "", ]
zipcodes <- zipcodes[zipcodes$PC6 != "", ]

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
