#libraries

library(here)
library(tidyverse)
library(magick)
library(dplyr)

#load data

nnd <- read.csv("data/number-of-natural-disaster-events.csv")
newsnd <- read.csv("data/news-coverage-of-disasters.csv")

# to check loading worked

head(nnd)
head(newsnd)

#first plot showing number of natural disasters recorded
#filter out data I want to focus on (all disasters, extreme weather, earthquake, flood, and drought)
#rest of the data are combined as "other"

#Group by year and entity, and sum the events

disasters_sum <-nnd %>% 
  group_by(Year, Entity) %>%
  summarise(events_sum = sum(Number.of.reported.natural.disasters))

#Sum "other" events 

other <- disasters_sum %>%
  filter(Entity %in% c("Fog", "Glacial lake outburst", "Dry mass movement", "Volcanic activity", "Extreme temperature", "Landslide")) %>% 
  group_by(Year) %>%
  summarise(events_sum = sum(events_sum)) %>% 
  mutate(Entity = "Other")

#Combine data frames

binded <- bind_rows(disasters_sum, other)

#sort by yeaar

binded <- binded %>% 
  arrange(Year)

#filter

nnd_filtered <- binded %>% 
  filter(Entity %in% c("All disasters","Extreme weather","Earthquake","Flood","Drought","Other"))

#save the data frame

write.csv(nnd_filtered, file = "processed/nnd_filtered.csv", row.names=FALSE)

#setting the order of legand

nnd_filtered$Entity <- 
  factor(nnd_filtered$Entity, levels = c("All disasters", "Flood", "Extreme weather", "Earthquake", "Drought","Other"))

#plot a graph

bestfit <- ggplot(nnd_filtered, aes (x = Year, y = events_sum, group = Entity)) + 
  geom_smooth (method = "gam", se = F, span = 2, aes (col = Entity), alpha = 0.5, size = 0.5) + 
  geom_line (alpha = 0.8, size = 0.8, aes (col = Entity))  + 
  ggtitle("Number of Natural Disaster Events") +
  labs(title = "Number of Recorded Natural Disasters",subtitle = "1900 to 2022", 
       x = "Year", y = "Number of events", 
       fill = "natural disaster types", color = "Disaster Category") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#save the image

ggsave("nnd_bestfit.png", path = "outputs/", plot = bestfit, width=15, height=10,dpi=300)

#second plot showing the relation between number of disasters and news percentage

#number of natural disaster in 2002
nnd2002 <- nnd %>% filter(Year == 2002)

#sort out nnd2002

nnd2002 <- nnd2002 [-1, -c(2,3)]

DMM <- nnd2002 %>%
  filter(Entity %in% c("Landslide", "Dry mass movement")) %>%
  summarise(Number.of.reported.natural.disasters = sum(Number.of.reported.natural.disasters)) %>% 
  mutate(Entity = "Dry mass movement")

nnd2002 <- bind_rows(DMM, nnd2002)
nnd2002 <- nnd2002 [-c(3,8),]

#sort out newsnd

newsnd <- newsnd [-c(1,2,6,7,10,12,13), -c(2,3)]

newsnd <- newsnd %>% rename(Share.in.news = Share.in.news..Eisensee.and.Strömberg.2007.)

newsnd <- newsnd %>% 
  mutate (Entity = fct_recode(Entity,"Extreme weather"= "Storm", 
                              "Extreme temperature" = "Cold wave", "Dry mass movement" = "Landslide", 
                              "Volcanic activity" = "Volcano", "Wildfire" = "Fire"))

#combine data frames

news_n_nd <- merge(newsnd, nnd2002, by="Entity")
news_n_nd <- print(arrange(news_n_nd, -Number.of.reported.natural.disasters))

#save data frame

write.csv(news_n_nd, file = "processed/news_n_nd.csv", row.names=FALSE)

#plot

news_n_relation <- ggplot(news_n_nd) +
  geom_col(aes(x=Entity, y=Number.of.reported.natural.disasters), 
           col = "dodgerblue4", fill= "dodgerblue4", alpha = 0.8) +
  geom_line(aes(x=Entity, y=Share.in.news, group=1), stat = "identity", 
            col = "steelblue2", size = 1) + 
  labs(title = "Number and News Percentage of Natural Disasters by Types", subtitle = "Year 2002") + 
  scale_x_discrete("Type of natural disasters", 
                   limits = c("Flood","Extreme weather","Earthquake","Drought","Wildfire",
                              "Dry mass movement","Extreme temperature","Volcanic activity")) +
  scale_y_continuous("Number of natural disasters", 
                     sec.axis=sec_axis(~.*0.01,name="Percentage of News", labels=scales::percent)) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y.left = element_text(face = "bold",color = "dodgerblue4"), 
        axis.title.y.right = element_text(face = "bold",color = "steelblue2"))

news_n_relation

#save 

ggsave("news_n_relation.png", path = "outputs/", plot = news_n_relation, width=15, height=10,dpi=300)
