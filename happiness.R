#------------------Load packages--------------
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("hablar")
install.packages("tidyr")
install.packages("reshape2")
install.packages("mapdata")
install.packages("hrbrthemes")
install.packages("viridis")

library(ggplot2)
library(dplyr)
library(hablar)
library(magrittr)
library(tidyverse)
library(tidyr)
library(knitr)
library(reshape2)
library(mapdata)
library(hrbrthemes)
library(viridis)

#------------------read in data, clean, and check--------------
# read in 
world <- read.csv("world-happiness-report-2021.csv")

# convert to local data frame
world <- tibble::as_tibble(world)

# create map data frame from base r
map <- map_data("world") 

#change column names
colnames(world) <- c("country", "regional.indicator", "ladder.score", "standard.error.ladder.score", "upper.whisker", "lower.whisker", "gdp.per.cap", "social.support", "life.exp", "freedom", "generosity", "perception.of.corruption", "dystopia.ladder.score", "explained.gdp", "explained.social.support", "explained.life.exp", "explained.freedom", "explained.generosity", "explained.corruption", "dystopia.residual")
colnames(map)[5] <- "country"

#check # of countries
length(unique(map$country)) #252 countries 
length(unique(world$country)) #149 countries
sort(unique(map$country)) == sort(unique(world$country))

#change united states to USA to match other data set
world <- world %>% 
  mutate(country=replace(country, country=="United States", "USA")) 

# merge map with country
map_data <- left_join(map, world, by="country")

#------------------explorations--------------

#bar chart for country regions
sort(unique(map_data$country))

regions.plot <- world %>%
  dplyr::count(regional.indicator, sort=T) %>%
  dplyr::mutate(
    regional.indicator=forcats::fct_rev(forcats::fct_inorder(regional.indicator))
  )

ggplot(regions.plot, aes(x=regional.indicator, y=n)) +
  geom_col(fill="seagreen3") +
  coord_flip() +
  geom_text(aes(label=n),
            hjust=2, color="white") +
  theme_light() + #setting a base theme and getting rid of stuff i don't want
  theme(
    panel.grid.major=element_blank(), 
    panel.border=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.y=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_text(size=13, face="bold"),
    line=element_blank()) 

#box plot for freedom, social support, generosity, and perception of corruption overall
factor.cols <- c("social.support", "generosity", "freedom", "perception.of.corruption")
box.plot <- gather(world, factor, value, factor.cols, factor_key=T)
  
box.plot <- box.plot %>%
  select(factor, value, regional.indicator)

ggplot(box.plot, aes(x=factor, y=value)) +
  geom_jitter(color="mediumpurple1") +
  geom_boxplot(alpha=.5) +
  scale_fill_viridis(discrete=T, alpha=0.6) 

#------------------viz--------------
#basic chart
ggplot(map_data, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=ladder.score), size=0)

#modified for style
ggplot(map_data, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=ladder.score), size=0) +
  scale_fill_gradient(name="Happiness Score") +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    rect=element_blank())




