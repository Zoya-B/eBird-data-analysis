obsbydate <- obs_filtered %>%
  mutate(month = month(observation_date, label = TRUE, abbr = TRUE))

plot(obsbydate$month) #n rows per month, combined checklists and species total

month_sum <- obsbydate %>%
  group_by(month) %>%
  summarize(Checklists = n_distinct(checklist_dt), 
            Richness = n_distinct(common_name), 
            Abundance = sum(observation_count))

month_sum <- pivot_longer(month_sum, -month, names_to = "measure", values_to = "values")

plot <- month_sum %>%
  ggplot()+
  geom_col(aes(x = month, y = values), fill = "skyblue")+
  facet_grid(rows = vars(measure), scales = "free")+
  labs(x = "Observation Month", y = "Values")+
  theme_bw()

print(plot) 

ggsave("monthly records.pdf", plot, width = 160, height = 140, units = "mm")
ggsave("monthly records.png", plot, width = 160, height = 140, units = "mm")

#summer and early fall (May - Oct) have highest values for all 3 measures

summerobs <- filter(obsbydate, month %in% c("May", "Jun", "Jul"))
fallobs <- filter(obsbydate, month %in% c("Aug", "Sep", "Oct"))

summerobs %>%
  summarize(Checklists = n_distinct(checklist_dt), 
            Richness = n_distinct(common_name), 
            Abundance = sum(observation_count))
fallobs %>%
  summarize(Checklists = n_distinct(checklist_dt), 
            Richness = n_distinct(common_name), 
            Abundance = sum(observation_count))

#summer and fall have 70/80 total checklists, both with 80+ species. will use fall period for comparison (40.6% of all obs)

fall_summary <- fallobs %>% group_by(site, timeline) %>%
  summarize(richness = n_distinct(common_name), checklists = n_distinct(checklist_dt), 
            avg_observers = round(mean(number_observers, na.rm = TRUE)), 
            avg_distance = mean(effort_distance_km, na.rm = TRUE),
            avg_duration = mean(duration_minutes, na.rm = TRUE))
fall_summary <- filter(fall_summary, !site %in% "LSG")
