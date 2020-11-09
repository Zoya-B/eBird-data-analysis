library(auk)
library(tidyverse)
library(dplyr)
library(ggplot2)

obs <- read.csv("Obs_complete.csv", header = TRUE, sep = ",")
obs <- obs[1:5852,] #file imported with empty rows from original unfiltered dataset
obs <- janitor::clean_names(obs)

obs$observation_date<-as.Date(obs$observation_date, format = "%d/%m/%Y")
obs$observation_count[obs$observation_count == "X"] <- "0.5"
obs$observation_count <- as.numeric(obs$observation_count)
obs$site <- as.factor(obs$site)
obs$timeline <- as.factor(obs$timeline)

obs$effort_distance_km[obs$protocol_type == "Stationary" ] <- 0
obs$effort_distance_km[obs$protocol_type == "CWC Point Count" | obs$protocol_type == "CWC Area Search"] <- 0
obs$effort_distance_km[obs$protocol_type == "Incidental" | obs$protocol_type == "Historical"] <- 0

obs$duration_minutes[obs$protocol_type == "Incidental" | obs$protocol_type == "Historical"] <- 0

obs$number_observers <- replace_na(obs$number_observers, 0)

total_richness <- n_distinct(obs$common_name)

summary <- obs %>% group_by(site) %>%
  summarize(richness = n_distinct(common_name), checklists = n_distinct(time_observations_started), 
            avg_observers = round(mean(number_observers, na.rm = TRUE)), 
            max_observers = max(number_observers, na.rm = TRUE),
            avg_distance = mean(effort_distance_km, na.rm = TRUE),
            max_distance = max(effort_distance_km, na.rm = TRUE), 
            avg_duration = mean(duration_minutes, na.rm = TRUE), 
            min_length = min(duration_minutes, na.rm = TRUE), 
            max_length = max(duration_minutes, na.rm = TRUE))

#to account for variability in detectability (https://cornelllabofornithology.github.io/ebird-best-practices/ebird.html#ebird-detect)
#filter for checklist with < 10 observers, < 5 hours long and <5 km travelled

obs_filtered <- obs %>% 
  filter(duration_minutes <= 5*60, effort_distance_km <= 5, number_observers <= 10)
#replaced NA distance and duration values, new n post-filter is 4800 vs 3669 when NA values filtered out
#replaced NA observer counts values, new n 5088 obs

summary_postfilter <- obs_filtered %>% group_by(site) %>%
  summarize(richness = n_distinct(common_name), checklists = n_distinct(time_observations_started), 
            avg_observers = round(mean(number_observers, na.rm = TRUE)), 
            max_observers = max(number_observers, na.rm = TRUE),
            avg_distance = mean(effort_distance_km, na.rm = TRUE),
            max_distance = max(effort_distance_km, na.rm = TRUE), 
            avg_duration = mean(duration_minutes, na.rm = TRUE), 
            min_length = min(duration_minutes, na.rm = TRUE), 
            max_length = max(duration_minutes, na.rm = TRUE))


lev_obs <- filter(obs_filtered, site %in% "LEV")
lev_obs %>% group_by(timeline) %>%
  summarize(checklists = n_distinct(time_observations_started), 
            richness = n_distinct(common_name), proportion = richness/70)

lsg_obs <- filter(obs_filtered, site %in% "LSG")
lsg_obs %>% group_by(timeline) %>%
  summarize(checklists = n_distinct(time_observations_started), 
            richness = n_distinct(common_name), proportion = richness/79)

mth_obs <- filter(obs_filtered, site %in% "MTH")
mth_obs %>% group_by(timeline) %>%
  summarize(checklists = n_distinct(time_observations_started), 
            richness = n_distinct(common_name), proportion = richness/59)
            