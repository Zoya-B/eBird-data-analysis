library(gtools)
library(RAM)
library(vegan)

lsgobs <- filter(obsbydate, site %in% "LSG")

lsgpre <- filter(lsgobs, timeline %in% "pre")
lsgpost <- filter(lsgobs, timeline %in% "post")
lsgpost2 <- semi_join(lsgpost, lsgpre, by = "month")

lsg2 <- bind_rows(lsgpre, lsgpost2)

lsg <- lsg2 %>% group_by(timeline, checklist_dt) %>%
  summarize(Richness = n_distinct(common_name), proportion = Richness/82, 
            Shannon = diversity(observation_count, index = "shannon"), 
            Simpson = diversity(observation_count, index = "simpson"))

t.test(lsg$proportion ~ lsg$timeline) # p = 1012
t.test(lsg$Richness ~ lsg$timeline) # same p, higher in post group
t.test(lsg$Shannon ~ lsg$timeline) #0.5811
t.test(lsg$Simpson ~ lsg$timeline) #0.793

lsglong <- pivot_longer(lsg, c(Richness, Shannon, Simpson), names_to = "measure", values_to = "values" )

lsg_sum <- lsg2 %>% group_by(common_name, timeline) %>%
  summarize(occurrence = n_distinct(checklist_dt),
            total = sum(observation_count), 
            avg = total/occurrence)
lsg_sp <- pivot_wider(lsg_sum, id_cols = common_name, names_from = timeline, 
                      values_from = avg)

lsg_sp <- na.replace(lsg_sp, 0)
lsg_sp <- na.omit(lsg_sp)

t.test(lsg_sp$post, lsg_sp$pre, paired = TRUE) # 0.000479

lsg_sp <- mutate(lsg_sp, diff = post-pre)
