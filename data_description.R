# Load necessary libraries
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(gridExtra)

# Perform data analysis on the Donation data
joined_data <- fread("datasets/cleaned_joined_data.csv", sep = ",")

#descriptive stats
nrow(joined_data)
summary(joined_data)

# amount of donations over time
amount_of_donations <- joined_data %>% group_by(Year, Week) %>% 
summarise(pledges=sum(pledge_count), online_donations=sum(donation_count)) %>% 
mutate(key=paste(Year, Week))

p1 <- ggplot(amount_of_donations, aes(key, group=1)) +
  geom_line(aes(y=pledges),color="#753500")+
  theme_classic() +
labs(x="Time", y="Sum of pledges") +
  theme(axis.text.x = element_blank()) +
    annotate("text", x=5, y=-20, label= "2019", size=4) +
  annotate("text", x=55, y=-20, label= "2020", size=4) +
  annotate("text", x=105, y=-20, label= "2021", size=4) +
  annotate("text", x=150, y=-20, label= "2022", size=4)  
  

p2 <-ggplot(amount_of_donations, aes(key, group=1)) +
  geom_line(aes(y=online_donations),color="#1cabe2")+
  theme_classic() +
  labs(x="Time", y="Sum of online donations")+
  theme(axis.text.x = element_blank()) +
  annotate("text", x=5, y=-500, label= "2019", size=4) +
  annotate("text", x=55, y=-500, label= "2020", size=4) +
  annotate("text", x=105, y=-500, label= "2021", size=4) +
  annotate("text", x=150, y=-500, label= "2022", size=4)  

grid.arrange(p1,p2, nrow=2)
ggsave("amount of donations per date.png")

#amount of external events over time
external_events_per_date <- external_events %>% group_by(datum) %>% 
summarise(count=n()) 

p3 <- ggplot(external_events_per_date, aes(datum,count,group=1)) +
  geom_line(color="#753500")+
  theme_classic() +
labs(x="Time", y="Amountof external events") 

#top 10 types of external events
external_events_per_type <- external_events %>% group_by(type) %>% 
summarise(count=n()) %>% filter(type !="") %>%
top_n(10) %>% arrange(-count)


p4 <- ggplot(data=external_events_per_type, aes(x=reorder(type,count), y=count)) +
  geom_bar(stat="identity", fill="#1cabe2")+
  theme_classic() +coord_flip() +
labs(x="Count of event", y="Event type") 

# count of events per region indicator
external_events_transpose<-external_events %>% group_by(datum) %>%
                summarise(Nederland_ind=sum(Nederland_ind),
                          Europe_ind=sum(Europe_ind),
                          Oceanie_ind=sum(Oceanie_ind),
                          Afrika_ind=sum(Afrika_ind),
                          Amerika_ind=sum(Amerika_ind),
                          Asie_ind=sum(Asie_ind),
                )

p5 <- ggplot(external_events_transpose, aes(datum, group=1)) +
  geom_line(aes(y=Nederland_ind),color="#753500")+
  geom_line(aes(y=Europe_ind),color="#0077AA")+
  geom_line(aes(y=Oceanie_ind),color="#1CABE2")+
  geom_line(aes(y=Afrika_ind),color="#CAF7FF")+
  geom_line(aes(y=Amerika_ind),color="#235952")+
  geom_line(aes(y=Asie_ind),color="#A72D6F")+
  theme_classic() +
  labs(x="Time", y="Amount of events in the region") 


# count of donations after a pledge/no pledge

# count of donations after an event



