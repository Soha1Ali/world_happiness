#------------------Load packages--------------
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("hablar")
install.packages("tidyr")
install.packages("reshape2")
install.packages("mapdata")

library(ggplot2)
library(dplyr)
library(hablar)
library(magrittr)
library(tidyverse)
library(tidyr)
library(knitr)
library(reshape2)
library(mapdata)

#------------------read in data, clean, and check--------------
# read in 
world <- read.csv("world_data.csv")
# convert to local data frame
world <- tibble::as_tibble(world)
# create map data frame from base r
map <- map_data("world") 
#change column names
colnames(world) <- c("rank", "country", "happiness.score", "whisker.high", "whisker.low", "dystopia", "gdp", "social.support", "life.exp", "free.choices", "generosity", "corruption")
colnames(map)[5] <- "country"
#check # of countries
length(unique(map$country)) #252 countries 
length(unique(world$country)) #146 countries
sort(unique(map$country)) == sort(unique(world$country))
#change united states to USA to match other data set
world <-world %>% 
  mutate(country=replace(country, country=="United States", "USA")) 
# merge map with country
map_data <- left_join(map, world, by="country")
map_data %>% #checking USA
  select(country, happiness.score) %>%
  filter(country=="USA")
# looking for inconsistencies
map_data %>%
  select(country, happiness.score) %>%
  filter(is.na(happiness.score)) %>%
  distinct(., country) #132 countries
#missing countries. idk what to do about these
map_data %>% 
  select(country, happiness.score) %>%
  filter(country %in% c("Madagascar", "Liberia", "Sudan", "South Sudan", "Greenland", "Lybia", "Mauritania", "Gambia", "Botswana", "eSwatini", "Chad", "Niger", "Chad", "Central African Republic", "Angola", "Somalia")) %>%
  distinct(., country, happiness.score)

#------------------viz--------------
#basic chart
ggplot(map_data, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=happiness.score), size=0)
#modified for style
ggplot(map_data, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=happiness.score), size=0) +
  scale_fill_gradient(name="Happiness Score", low="red", high="darkgreen", na.value="gray") +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    rect=element_blank())




