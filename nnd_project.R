#install packages just in case
#if installed already select "no"

install.packages ("tidyverse")
install.packages("magick")

#libraries

library(tidyverse)
library(gganimate)
library(magick)
library(dplyr)

#load data

nnd <- read.csv("data/number-of-natural-disaster-events.csv")

# to check loading worked

head(nnd)

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

