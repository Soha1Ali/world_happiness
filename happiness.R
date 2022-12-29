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

#------------------read in data--------------
# read in 
world <- read.csv("world_data.csv")
# convert to local data frame
world <- tibble::as_tibble(world)
# merge map data set from R with my data set
mapdata_base <- map_data("world")
countries_mapdata <- unique(mapdata_base$region)
world$Country[world$Country == "United States"] <- "USA"
colnames(mapdata)[5] = "Country" 
mapdata <- left_join(mapdata, world, by="Country")
unique(mapdata$Country)

mapdata %>%
  select(Country, Happiness.score.x) %>%
  filter(Country=="USA")

mapdata_base %>%
  select(region, lat, long) %>%
  filter(region=="USA")

#------------------viz--------------
#basic
map1 <- ggplot(mapdata, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Happiness.score), color="black", size=0)
#modified for style
map2 <- map1 + scale_fill_gradient(name="Happiness Score", low="blue", high="red", na.value="gray") +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    rect=element_blank()
  )




