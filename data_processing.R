library(auk)
library(tidyverse)
library(dplyr)
library(ggplot2)

obs <- read.csv("Obs_by_date.csv", header = TRUE, sep = ",")
obs <- janitor::clean_names(obs)

obs$observation_count <- as.integer(obs$observation_count)
obs$site <- as.factor(obs$site)
obs$timeline <- as.factor(obs$timeline)
