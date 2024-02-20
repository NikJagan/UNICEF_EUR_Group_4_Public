# Load necessary libraries
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(gridExtra)

# Perform data analysis on the Donation data
joined_data <- fread("datasets/cleaned_joined_data.csv", sep = ",")
not_aggregated_data <- fread("datasets/not_aggregated_data_description.csv", sep = ",")
external_events <- fread("datasets/external_events_data.csv", , sep = ",")
migration_data <- fread("datasets/Population_migrationbackground_postcode.csv", sep = ";")

#descriptive stats
nrow(joined_data)
summary(joined_data)
sum(joined_data$pledge_ind)
sum(joined_data$online_donation_count)
names(joined_data)

# amount of donations over time
not_aggregated_data$DATE_external_event_end <- ymd(not_aggregated_data$DATE_external_event_end)
not_aggregated_data$Month_Yr <- format(as.Date(not_aggregated_data$DATE_external_event_end), "%Y-%m")
amount_of_donations <- not_aggregated_data %>% group_by(Month_Yr) %>% 
summarise(pledges=sum(PLEDGE_ID), online_donations=sum(GIFT_ID))

p1 <- ggplot(amount_of_donations, aes(Month_Yr, group=1)) +
  geom_line(aes(y=pledges),color="#753500")+
  theme_classic() +
labs(x="Time (months)", y="Sum of pledges") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p2 <-ggplot(amount_of_donations, aes(Month_Yr, group=1)) +
  geom_line(aes(y=online_donations),color="#1cabe2")+
  theme_classic() +
  labs(x="Time (months)", y="Sum of online donations") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

 g <- arrangeGrob(p1,p2, nrow=2) 
ggsave("amount of donations per date.png", g)

#amount of external events over time
external_events$Month_Yr <- format(as.Date(external_events$datum), "%Y-%m")
external_events <- na.omit(external_events)
external_events_per_date <- external_events %>% group_by(Month_Yr) %>% 
summarise(count=n()) 

p3 <- ggplot(external_events_per_date, aes(Month_Yr,count,group=1)) +
  geom_line(color="#100f0f")+
  theme_classic() +
labs(x="Time (months)", y="Amount of external events") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#check whether these events are different
external_events_types <- external_events %>% mutate(type_new=case_when(
                                                        type=="bosbrand" ~ "bosbrand",
                                                        type=="brand" ~ "bosbrand",
                                                        type=="oorlog"  ~ "oorlog",
                                                        type=="tyfoon" ~ "tyfoon",
                                                        type=="orkaan" ~ "orkaan",
                                                        type=="bosbranden" ~ "bosbrand",
                                                        type=="overstroming" ~ "overstroming",
                                                        type=="overstromingen" ~ "overstroming",
                                                        type=="coronavirus"~ "coronavirus",
                                                        type=="cycloon"~ "cycloon",
                                                        .default="overig"
                                                        ))%>% group_by(Month_Yr, type_new) %>% 
summarise(count=n()) 
p3_v2 <- ggplot(external_events_types, aes(Month_Yr,count,group=1, color=type_new)) +
  geom_line()+
  theme_classic() +
labs(x="Time (months)", y="Amount of external events") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


#top 10 types of external events
external_events_per_type <- external_events %>% group_by(type) %>% 
summarise(count=n()) %>% filter(type !="") %>%
top_n(10) %>% arrange(-count)

p4 <- ggplot(data=external_events_per_type, aes(x=reorder(type,count), y=count)) +
  geom_bar(stat="identity", fill="#1cabe2")+
  theme_classic() +coord_flip() +
labs(x="Count of event", y="Event type") 

 g <- arrangeGrob(p3,p4, ncol=2) 
ggsave("external events description.png",g)

 g <- arrangeGrob(p1,p2,p3, nrow=3) 
ggsave("all line plots.png", g)

# count of events per region indicator
external_events_transpose<-external_events %>% group_by(Month_Yr) %>%
                summarise(Nederland_ind=sum(Nederland_ind),
                          Europe_ind=sum(Europe_ind),
                          World_ind=sum(World_ind)
                )

colors <- c("Netherlands" = "#753500", "Europe" = "#374955", "World" = "#1CABE2")
p5 <- ggplot(external_events_transpose, aes(Month_Yr, group=1)) +
  geom_line(aes(y=Nederland_ind,color="Netherlands"), size=1.5)+
  geom_line(aes(y=Europe_ind,color="Europe"), size=1.5)+
  geom_line(aes(y=World_ind,color="World"), size=1.5) +
  theme_classic() +
  labs(x="Time", y="Amount of events in the region", color = "Legend") +
  scale_color_manual(values = colors)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("external events by region.png")

# count of donations after a pledge/no pledge
carryover_effect <- joined_data %>% mutate(pledge_ind = ifelse(pledge_count>0,  "Yes", "No")) %>%
group_by(pledge_ind) %>% summarise(donation_count=sum(online_donation_count))

p6 <- ggplot(data=carryover_effect, aes(x=pledge_ind, y=donation_count)) +
  geom_bar(stat="identity", fill="#1CABE2")+
  theme_classic() +
  labs(x="Pledge was made", y="Count of online donations") +
   theme(axis.text.y=element_blank(),
        axis.ticks=element_blank())
p6
ggsave("donation count after pledge.png")

# count of donations after an event
events_effect <- joined_data %>% mutate(event_ind = ifelse((Nederland_ind==1|Europe_ind==1|World_ind==1), "Yes", "No")) %>%
group_by(event_ind) %>% summarise(donation_count=sum(online_donation_count))

p7 <- ggplot(data=events_effect, aes(x=event_ind, y=donation_count)) +
  geom_bar(stat="identity", fill="#1CABE2")+
  theme_classic() +
  labs(x="Event happened", y="Donations made") +
   theme(axis.text.y=element_blank(),
        axis.ticks=element_blank())
p7
ggsave("donation count after event.png")

#different demographic groups count in 2022
#Remove irrelevant Rows
migration_data$Geslacht <- NULL
migration_data <- migration_data[Postcode != "Niet in te delen" & Postcode != "Nederland"]
migration_data$Migratieachtergrond <- trimws(migration_data$Migratieachtergrond)
migration_data <- migration_data[(Migratieachtergrond != "Totaal" & Migratieachtergrond != "Met migratieachtergrond" & Migratieachtergrond != "Overige westerse migratieachtergrond" & Migratieachtergrond != "Overige niet-westerse migratieachterg..." & Migratieachtergrond != "Westerse migratieachtergrond" & Migratieachtergrond != "Niet-westerse migratieachtergrond" & Migratieachtergrond != "Europese Unie (excl. Nederlandse acht..."),]
migration_data <- migration_data %>% mutate_if(is.character, ~na_if(., ""))
migration_data <- migration_data[complete.cases(migration_data), ] #remove missing values

#create shorter strings
conditions <- c("(voormalige) Nederlandse Antillen, Aruba", "Europa (excl. Nederlandse achtergrond)")
replacement_values <- c("Aruba", "Europa (excl. NL)")
 
# Use replace() to replace the names in the 'Names' column
migration_data$Migratieachtergrond <- replace(migration_data$Migratieachtergrond, migration_data$Migratieachtergrond %in% conditions, replacement_values)
migration_data <- migration_data %>% rename(Population="Bevolking (aantal)")
migration_data_22 <- migration_data[Perioden==2022]
migration_data_total <- migration_data_22 %>% group_by(Migratieachtergrond, Perioden) %>% summarise(Population=sum(as.numeric(Population)))

p8 <- ggplot(migration_data_total, aes(y=Population, x=reorder(Migratieachtergrond, -Population))) + 
    geom_bar(fill="#1CABE2", stat="identity") +
    theme_classic() + labs(x="Cultural background", y="Population") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.text.y=element_blank(),)

ggsave("population by migration background.png")

#external events proximities
external_events_per_region <- external_events %>% mutate(proximity=case_when(
                                                        Nederland_ind==1 ~ "Netherlands",
                                                        Marokko_ind==1 ~ "Morocco",
                                                        Turkije_ind==1 ~ "Turkey",
                                                        Belgie_ind==1 ~ "Belgium",
                                                        Duitsland_ind==1 ~ "Germany",
                                                        Indonesie_ind==1 ~ "Indonesia",
                                                        Polen_ind==1 ~ "Poland",
                                                        Suriname_ind==1 ~ "Surinam",
                                                        Oceanie_ind==1 ~ "Oceania",
                                                        Afrika_ind==1 ~ "Africa",
                                                        Amerika_ind==1 ~ "America",
                                                        Asie_ind==1 ~ "Asia")) %>% 
                                                        group_by(proximity)  %>% 
                                                        filter(!is.na(proximity))  %>% 
                                                        summarise(count=n()) %>% 
                                                        arrange(-count)

p4 <- ggplot(data=external_events_per_region, aes(x=reorder(proximity,count), y=count)) +
  geom_bar(stat="identity", fill="#1cabe2")+
  theme_classic() +coord_flip() +
labs(x="Count of events", y="Event region") 
ggsave("events by region.png")

#histogram of injured people
external_events$slachtoffers <- as.integer(external_events$slachtoffers)
external_events$slachtoffers [is.na(external_events$slachtoffers )] <- 0
external_events_cas <- external_events %>% mutate(casualty_ind=ifelse(slachtoffers>0, 1,0)) %>% group_by(casualty_ind) %>% summarise(count=n())
p<-ggplot(external_events, aes(x=slachtoffers)) + 
  geom_histogram(fill="#1cabe2")+
    theme_classic() + 
labs(x="Causalties count", y="") 
p


#discover which article corresponds to an observation
#find the observation
migration$Postcode <- as.numeric(migration$Postcode)
not_aggregated_data <- left_join(not_aggregated_data, migration, by = c("Year", "Postcode"))
observation <- not_aggregated_data[which(Bevolking>=1.685358826079 & Bevolking<=1.68535882608),]
observation <- observation[which(Postcode==5658),]
events_that_happened <- observation %>% group_by(datum, land, type) %>% summarise(count=n())
head(events_that_happened, 16)


#T-test to test for cannibalization of pledges
t.test(online_donation_count~pledge_ind, full_data)
t.test(total_donations_count~pledge_ind, full_data)

