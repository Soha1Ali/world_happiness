#------------------Load packages--------------
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("hablar")
#install.packages("tidyr")
#install.packages("reshape2")
#install.packages("mapdata")
#install.packages("hrbrthemes")
#install.packages("viridis")
#install.packages("knitr")
#install.packages("treemap")

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
library(treemap)
library(ggrepel)

#------------------READ AND PREPARE DATA--------------

# import world happiness report 2021 and merge with map coordinate df
world <- read.csv("world-happiness-report-2021.csv") 
world <- tibble::as_tibble(world) 
map <- map_data("world") # map data frame from base r

colnames(world) <- c("country", "regional.indicator", "ladder.score", "standard.error.ladder.score", 
                     "upper.whisker", "lower.whisker", "gdp.per.cap", "social.support", "life.exp", 
                     "freedom", "generosity", "perception.of.corruption", "dystopia.ladder.score", 
                     "explained.gdp", "explained.social.support", "explained.life.exp", "explained.freedom", 
                     "explained.generosity", "explained.corruption", "dystopia.residual") #change column names

colnames(map)[5] <- "country" 

world <- world %>% mutate(country=replace(country, country=="United States", "USA"),
                          country=replace(country, country=="Hong Kong S.A.R. of China", "Hong Kong"))

world.map <- left_join(map, world, by="country") # merge map with world

#------------------EXPLORATIONS--------------

# COUNTRY COUNT BY REGION-------------------------------------------------------

# data
world %>%
  count(regional.indicator, sort=T) %>%
  mutate(regional.indicator=forcats::fct_rev(forcats::fct_inorder(regional.indicator)))

# base viz
region.bar <- ggplot(region.bar.data, aes(x=regional.indicator, y=n)) + 
  geom_col() +
  coord_flip() +
  # base theme 
  theme_ipsum() +
  theme(
    axis.title.y=element_blank(), 
    axis.title.x=element_blank(),
    axis.text.x=element_blank()) +
  # column labels 
  geom_text(aes(label=n), hjust=2, color="white") +
  # titles themes
  ggtitle("country count by region") +
  theme(plot.title=element_text(size=15, hjust=.3, face="bold"))


treemap(region.bar.data,
        # data
        index="regional.indicator",
        vSize="n",
        type="index",
        
        # main
        title="",
        palette="Dark2",
        
        # borders
        border.col=c("black"),             
        border.lwds=1,
        
        # theme
        fontsize.labels=10,
        inflate.labels=T,
        align.labels=c("left", "top"),
        fontcolor.labels="white",
        overlap.labels=0.5,
        fontface.labels=1
        )

#-------------------------------------------------------------------------------

# GLOBAL FACTOR DISTRIBUTIONS---------------------------------------------------

# make data long form with gather()
factor.cols <- c("social.support", "generosity", "freedom", "perception.of.corruption") 
box.plot.data <- gather(world, factor, value, factor.cols, factor_key=T)

# base viz
distribution.global.box <- ggplot(box.plot.data, aes(x=factor, y=value)) + # box plot
  geom_jitter() +
  geom_boxplot(alpha=.5) +
  # base theme 
  theme_ipsum() +
  theme(
    axis.title.y=element_text(hjust=.5, vjust=4, size=15), 
    axis.title.x=element_text(hjust=.5, vjust=-3, size=15)) +
  # titles themes
  ggtitle("distribution: freedom, social support, generosity, and perception of corruption") +
  theme(plot.title=element_text(size=15, hjust=.5, face="bold"))

# base viz
distribution.global.violin <- ggplot(box.plot.data, aes(x=factor, y=value, fill=factor)) + # violin + box plot
  geom_violin() +
  geom_boxplot(width=0.1, alpha=0.2, color="pink") +
  # base theme 
  scale_fill_viridis(discrete=T) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title=element_text(size=20, hjust=.5,)) +
  # titles themes
  ggtitle("distributions") +
  xlab("factors") +
  theme(
    axis.title.y=element_text(hjust=.5, vjust=4, size=15), 
    axis.title.x=element_text(hjust=.5, vjust=-3, size=15))

#-------------------------------------------------------------------------------

# GDPs--------------------------------------------------------------------------

# highest GDPs table
gdp.plot.h <- world %>%
  select(country, regional.indicator, gdp.per.cap) %>%
  arrange(desc(gdp.per.cap)) %>%
  slice(1:10)

# lowest GDPs table
gdp.plot.l <- world %>%
  select(country, regional.indicator, gdp.per.cap) %>%
  arrange(gdp.per.cap) %>%
  slice(1:10)

# bind lowest and highest GDPs tables together
gdp.plot <- gdp.plot.h %>% bind_rows(.,gdp.plot.l)

# arrange values from low to high
gdp.plot <- gdp.plot %>% 
  arrange(gdp.per.cap) %>%
  select(country, gdp.per.cap)

gdp.plot$country <- factor(gdp.plot$country, levels=gdp.plot$country[order(gdp.plot$gdp.per.cap)])

# set colors
gdp.plot <- gdp.plot %>%
  mutate(
    color=case_when(
      row_number() == 1:10 ~ "coral2",
      TRUE ~ "seagreen3"
    ))

# base viz
ggplot(gdp.plot, aes(country, y=gdp.per.cap, color=color, fill=color)) +
  geom_point() +
  geom_segment(aes(x=country, xend=country, y=0, yend=gdp.per.cap)) +
  geom_label(aes(label=gdp.per.cap), size=3, hjust=1.2, color="white", fontface="bold") + 
  coord_flip() +
  # base theme
  theme_classic() +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    axis.text.y=element_text(size=10, face="bold", hjust=1),
    panel.grid.major=element_blank(), 
    panel.grid.minor=element_blank(), 
    panel.border=element_blank(),
    line=element_blank()) +
  theme(legend.position="none") +
  # titles themes
  ggtitle("Highest and Lowest GDPs in 2021") + ylab("Life Expectancy") +
  theme(plot.title=element_text(size=13, hjust=.4, face="bold"))

#-------------------------------------------------------------------------------

# LIFE EXPECTANCIES-------------------------------------------------------------

# highest life exp table
life.plot.h <- world %>%
  select(country, regional.indicator, life.exp) %>%
  arrange(desc(life.exp)) %>%
  slice(1:10)

# lowest life exp table
life.plot.l <- world %>%
  select(country, regional.indicator, life.exp) %>%
  arrange(life.exp) %>%
  slice(1:10)

# bind highest and lowest life exp
life.plot.data <- life.plot.h %>% bind_rows(.,life.plot.l)

# arrange values from low to high
life.plot.data <- life.plot.data %>% 
  arrange(life.exp) %>%
  select(country, life.exp)

life.plot.data$country <- factor(life.plot.data$country, levels=life.plot.data$country[order(life.plot.data$life.exp)])

# set colors
life.plot <- life.plot.data %>%
  mutate(
    color=case_when(
      row_number() == 1:10 ~ "coral2",
      TRUE ~ "seagreen3"
    ))

# base viz
ggplot(life.plot, aes(country, y=life.exp, color=color, fill=color)) +
  geom_col(color="gray") +
  geom_label(aes(label=life.exp), size=3, hjust=1.2, color="white", fontface="bold") + 
  coord_flip() +
  # base theme
  theme_classic() + #setting a base theme and getting rid of stuff i don't want
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.x=element_blank(),
    panel.grid.major=element_blank(), 
    panel.border=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    line=element_blank()) +
  theme(legend.position="none") +
  # titles themes
  ggtitle("Highest and Lowest Life Expectancies in 2021") + ylab("Life Expectancy") +
  theme(plot.title=element_text(size=13, hjust=.4, face="bold"))

# base viz
ggplot(life.plot, aes(country, y=life.exp, color=color)) +
  geom_point() +
  geom_segment(aes(x=country, xend=country, y=0, yend=life.exp)) +
  geom_label(aes(label=life.exp), hjust=1.5, size=3, color="black") + 
  coord_flip() +
  # base theme
  theme_classic() +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    axis.text.y=element_text(size=10, face="bold", hjust=1),
    panel.grid.major=element_blank(), 
    panel.grid.minor=element_blank(), 
    panel.border=element_blank(),
    line=element_blank()) +
  theme(legend.position="none") +
  # titles themes
  ggtitle("Highest and Lowest Healthy Life Expectancies in 2021") + ylab("Life Expectancy") +
  theme(plot.title=element_text(size=13, hjust=.4, face="bold"))

#-------------------------------------------------------------------------------

# MAPS--------------------------------------------------------------------------

# life expectancy 

world.map.1 <- world.map

# set ranges
world.map.1 <- world.map.1 %>%
  mutate(
    life.exp.range=case_when(
      life.exp >= 75 ~ "Greater than 75",
      life.exp >= 70 ~ "Between 70 and 75",
      life.exp >= 65 ~ "Between 65 and 70",
      life.exp >= 60 ~ "Between 60 and 65",
      life.exp >= 55 ~ "Between 55 and 60",
      life.exp >= 50 ~ "Between 50 and 55",
      life.exp >= 45 ~ "Between 45 and 50",
      TRUE ~ "NA"
    ))

# set color palette
pallete.0 <- c("#9575CD", "#7E57C2", "#673AB7", "#5E35B1", "#512DA8", "#4527A0", "#311B92", "#999999")

# base viz life expectancy 
ggplot(world.map.3, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=life.exp), size=0) +
  scale_fill_gradient(name="Life Expectancy") +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    rect=element_blank())

# base viz life expectancy with ranges and pallete.0
ggplot(world.map.1, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=life.exp.range), linewidth=.1, color="black") +
  scale_fill_manual(values=pallete.0, name="Life Expectancy Ranges") +
  # base theme
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    rect=element_blank())

# happiness score

# set ranges 
world.map.3 <- world.map.1 %>%
  mutate(
    happiness.score.range.2=case_when(
      ladder.score >= 6.98 ~ "6.98-7.59",
      ladder.score >= 6.51 ~ "6.51-6.98",
      ladder.score >= 5.98 ~ "5.98-6.51",
      ladder.score >= 5.75 ~ "5.75-5.98",
      ladder.score >= 4.90 ~ "5.20-5.75",
      ladder.score >= 4.90 ~ "4.90-5.20",
      ladder.score >= 4.68 ~ "4.68-4.90",
      ladder.score >= 4.33 ~ "4.33-4.68",
      ladder.score >= 3.90 ~ "3.90-4.33",
      ladder.score >= 2.84 ~ "2.84-3.90",
      TRUE ~ "No data"
    ))

# set color palettes
pallete.1 <- c("#9575CD", "#7E57C2", "#673AB7", "#5E35B1", "#512DA8", "#4527A0", "#999999")
pallete.2 <- c("#016837", "#16984e", "#66be64", "#a6d16a", "#d7e58c", "#fddf8a", "#faac61", "#f26c43", "#d73026", "#a41d29", "white")
pallete.3 <- c("#a41d29", "#d73026", "#f26c43", "#faac61", "#fddf8a", "#d7e58c", "#a6d16a", "#66be64", "#16984e", "white")

# base viz for happiness score with ranges and pallete.3
ggplot(world.map.3, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=happiness.score.range.2), linewidth=.1, color="black") +
  scale_fill_manual(values=pallete.3, name="Happiness Score Ranges") +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    rect=element_blank())

#-------------------------------------------------------------------------------

# DESCRIPTIVES BY REGION--------------------------------------------------------

# make data long form
factor.cols <- c("social.support", "generosity", "freedom", "perception.of.corruption")
box.plot.data <- gather(world, factor, value, factor.cols, factor_key=T)

# base viz
ggplot(box.plot.data, aes(x=factor, y=value, fill=factor)) +
  geom_violin() +
  geom_boxplot(width=0.1, alpha=0.2, color="white") +
  scale_fill_viridis(discrete=T) +
  # base theme
  theme_classic() +
  theme(
    legend.position="none",
    plot.title=element_text(size=20, hjust=.5),
    axis.text.x=element_text(size=10)) +
  # titles themes
  ggtitle("distributions of four key happiness factors") +
  xlab("factors") +
  theme(
    axis.title.y=element_text(hjust=.5, vjust=4, size=15), 
    axis.title.x=element_text(hjust=.5, vjust=-3, size=15))

# CORRELATIONS------------------------------------------------------------------

# life expectancy by happiness score--------------------------------------------

# linear regression model
a.regression <- lm(ladder.score ~ life.exp, data=world)
summary(a.regression)
# the least squares line equation: y = .1219x - 2.3954
# the relationship between variables is stat sig (p = < .0001)
# life exp can explain 59% of the variation in happiness scores (r2 = 0.59)
# in other words, there is a 59% reduction in variation of happiness scores once we take life exp into account
# the r2 value is stat sig (p = < .0001)

# group european countries together
world.1 <- world %>%
  mutate(
    region=case_when(
      regional.indicator=="Central and Easter Europe" ~ "Europe",
      regional.indicator=="Western Europe" ~ "Europe",
      TRUE ~ "Non-Europe"))

# base viz
ggplot(world.1, aes(x=life.exp, y=ladder.score)) +
  geom_point(aes(color=region), size=5) + 
  geom_point(pch=21, color="black", size=5, alpha=.5) +
  # base theme
  theme_classic() +
  theme(
    axis.title.y=element_text(hjust=.5, vjust=4, size=15), 
    axis.title.x=element_text(hjust=.5, vjust=-3, size=15)) +
  # titles themes
  ggtitle("healthy life expectancy by happiness score") +
  theme(plot.title=element_text(size=15, hjust=.5, face="bold"))

# life expectancy by happiness score grouped by region--------------------------

# get regional averages
region.averages <- world %>%
  group_by(regional.indicator) %>%
  summarize(mean.life.exp=mean(life.exp), 
            mean.happiness=mean(ladder.score))

# base viz
ggplot(region.averages, aes(x=mean.life.exp, y=mean.happiness, color=regional.indicator)) +
  geom_point(size=10) + 
  geom_point(pch=21, color="black", alpha=.5, size=10) +
  geom_label_repel(aes(label=regional.indicator), box.padding=.35) +
  # base theme
  theme_classic() +
  theme(
    legend.position="none",
    axis.title.y=element_text(hjust=.5, vjust=4, size=15), 
    axis.title.x=element_text(hjust=.5, vjust=-3, size=15)) +
  # titles themes
  ggtitle("healthy life expectancy by happiness score grouped by region") +
  theme(plot.title=element_text(size=15, hjust=.5, face="bold"))

#-------------------------------------------------------------------------------


