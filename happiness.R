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

#check # of countries in each df
length(unique(map$country)) #252 countries 
length(unique(world$country)) #149 countries

#change united states to USA so dfs match
world <- world %>% 
  mutate(country=replace(country, country=="United States", "USA")) 

# merge map with country
map_data <- left_join(map, world, by="country")

#------------------explorations--------------

#bar chart for country regions
country.bar.data <- world %>%
  dplyr::count(regional.indicator, sort=T) %>%
  dplyr::mutate(regional.indicator=forcats::fct_rev(forcats::fct_inorder(regional.indicator)))

country.bar <- ggplot(country.bar.data, aes(x=regional.indicator, y=n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label=n), hjust=2, color="white") +
  theme_ipsum() 

#box plot for freedom, social support, generosity, and perception of corruption overall
factor.cols <- c("social.support", "generosity", "freedom", "perception.of.corruption")

box.plot.data <- gather(world, factor, value, factor.cols, factor_key=T)

box.plot <- ggplot(box.plot.data, aes(x=factor, y=value)) +
  geom_jitter() +
  geom_boxplot(alpha=.5) 

#highest and lowest GDP 
gdp.plot.h <- world %>%
  select(country, regional.indicator, gdp.per.cap) %>%
  arrange(desc(gdp.per.cap)) %>%
  slice(1:10)

gdp.plot.l <- world %>%
  select(country, regional.indicator, gdp.per.cap) %>%
  arrange(gdp.per.cap) %>%
  slice(1:10)

gdp.plot <- gdp.plot.h %>% bind_rows(.,gdp.plot.l)

gdp.plot <- gdp.plot %>% 
  arrange(gdp.per.cap) %>%
  select(country, gdp.per.cap)

gdp.plot$country <- factor(gdp.plot$country, levels=gdp.plot$country[order(gdp.plot$gdp.per.cap)])

ggplot(gdp.plot, aes(gdp.plot$country, y=gdp.per.cap)) +
  geom_point() +
  geom_segment(aes(x=country, xend=country, y=0, yend=gdp.per.cap))

ggplot(gdp.plot, aes(country, y=gdp.per.cap)) +
  geom_point() +
  geom_segment(aes(x=country, xend=country, y=0, yend=gdp.per.cap)) +
  coord_flip() 

#highest and lowest life exp

life.plot.h <- world %>%
  select(country, regional.indicator, life.exp) %>%
  arrange(desc(life.exp)) %>%
  slice(1:10)

life.plot.l <- world %>%
  select(country, regional.indicator, life.exp) %>%
  arrange(life.exp) %>%
  slice(1:10)

life.plot.data <- life.plot.h %>% bind_rows(.,life.plot.l)

life.plot.data <- life.plot.data %>% 
  arrange(life.exp) %>%
  select(country, life.exp)

life.plot.data$country <- factor(life.plot.data$country, levels=life.plot$country[order(life.plot.data$life.exp)])

life.plot <- life.plot.data %>%
  mutate(
    color=case_when(
      row_number() == 1:10 ~ "coral2",
      TRUE ~ "seagreen3"
    ))

ggplot(life.plot, aes(country, y=life.exp, color=color)) +
  geom_point() +
  geom_segment(aes(x=country, xend=country, y=0, yend=life.exp)) +
  geom_text(aes(label=life.exp), hjust=-.2) +
  coord_flip() +
  theme_light() +
  theme(
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_text(size=10, face="bold", hjust=1),
    panel.grid.major=element_blank(), 
    panel.border=element_blank(),
    line=element_blank()) +
  
  theme(legend.position="none") +
  
    ggtitle("Highest and Lowest Life Expectancies in 2021") + ylab("Life Expectancy") +
    theme(plot.title=element_text(size=15, hjust=.4, face="bold"))

#map for happiness

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

# descriptive statistics for each factor by region

region.medians <- world %>%
  group_by(regional.indicator) %>%
  summarize(median.social.support=median(social.support), 
            median.freedom=median(freedom), 
            median.generosity=median(generosity),
            median.corruption=median(perception.of.corruption))

factor.cols <- c("social.support", "generosity", "freedom", "perception.of.corruption")

box.plot.data <- gather(world, factor, value, factor.cols, factor_key=T)

ggplot(box.plot.data, aes(x=factor, y=value, fill=factor)) +
  geom_violin() +
  geom_boxplot(width=0.1, alpha=0.2, color="gray60") +
  scale_fill_viridis(discrete=T) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title=element_text(size=20, hjust=.5,)
  ) +
  ggtitle("distributions: social support, generosity, freedom, and corruption") +
  xlab("factors") +
  theme(
    axis.title.y=element_text(size=15, hjust=.5),
    axis.title.x=element_text(size=15, hjust=.5)
  )

# violin for each region

ggplot(box.plot.data, aes(x=factor, y=value, fill=regional.indicator)) +
  geom_violin() +
  theme_ipsum() +
  theme(
    plot.title=element_text(size=20, hjust=.5,)
  ) +
  ggtitle("distributions: social support, generosity, freedom, and corruption") +
  xlab("factors") +
  theme(
    axis.title.y=element_text(size=15, hjust=.5),
    axis.title.x=element_text(size=15, hjust=.5)
  )

# life expectancy by happiness score with gdp

europe <- world %>%
  filter(
    regional.indicator=="Central and Eastern Europe" |
    regional.indicator=="Western Europe")

world.1 <- world %>%
  mutate(
    color=case_when(
      regional.indicator=="Central and Eastern Europe" ~ "coral2",
      regional.indicator=="Western Europe" ~ "coral2",
      TRUE ~ "gray30"
    )
  )

ggplot(world.1, aes(x=life.exp, y=ladder.score, size=gdp.per.cap)) +
  geom_point(aes(color=color)) + 
  geom_point(pch=21, color="black", alpha=.5)

# freedom by corruption with happiness score

europe <- world %>%
  filter(
    regional.indicator=="Central and Eastern Europe" |
      regional.indicator=="Western Europe")

world.1 <- world %>%
  mutate(
    color=case_when(
      regional.indicator=="Central and Eastern Europe" ~ "coral2",
      regional.indicator=="Western Europe" ~ "coral2",
      TRUE ~ "gray30"
    )
  )

ggplot(world.1, aes(x=freedom, y=perception.of.corruption, size=ladder.score)) +
  geom_point(aes(color=color)) + 
  geom_point(pch=21, color="black", alpha=.5)

# life exp by happiness score with regional indicator

region.medians <- world %>%
  group_by(regional.indicator) %>%
  summarize(median.social.support=median(social.support), 
            median.freedom=median(freedom), 
            median.generosity=median(generosity),
            median.corruption=median(perception.of.corruption))

region.averages <- world %>%
  group_by(regional.indicator) %>%
  summarize(mean.life.exp=mean(life.exp), 
            mean.happiness=mean(ladder.score))

ggplot(region.averages, aes(x=mean.life.exp, y=mean.happiness, color=regional.indicator)) +
  geom_point(size=10) + 
  geom_point(pch=21, color="black", alpha=.5, size=10)


