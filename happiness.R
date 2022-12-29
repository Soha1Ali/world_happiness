#------------------Load packages--------------
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("hablar")
install.packages("tidyr")
install.packages("reshape2")

library(ggplot2)
library(dplyr)
library(hablar)
library(magrittr)
library(tidyverse)
library(tidyr)
library(knitr)
library(reshape2)

#------------------read in data--------------
# read in 
world <- read.csv("world_data.csv")
# convert to local data frame
world <- tibble::as_tibble(world)
