library(gtools)
library(RAM)
library(vegan)


levobs <- filter(fallobs, site %in% "LEV")

lev <- levobs %>% group_by(timeline, checklist_dt) %>%
  summarize(Richness = n_distinct(common_name), proportion = Richness/80, 
            Shannon = diversity(observation_count, index = "shannon"), 
            Simpson = diversity(observation_count, index = "simpson"))

t.test(lev$proportion ~ lev$timeline) # p = 0.086
t.test(lev$Richness ~ lev$timeline) # same p, higher in post group
t.test(lev$Shannon ~ lev$timeline) #0.02361
t.test(lev$Simpson ~ lev$timeline) #0.1301

levlong <- pivot_longer(lev, c(Richness, Shannon, Simpson), names_to = "measure", values_to = "values" )

lev_sum <- levobs %>% group_by(common_name, timeline) %>%
  summarize(occurrence = n_distinct(checklist_dt),
            total = sum(observation_count), 
            avg = total/occurrence)
lev_sp <- pivot_wider(lev_sum, id_cols = common_name, names_from = timeline, 
                      values_from = avg)

lev_sp <- na.replace(lev_sp, 0)
lev_sp <- na.omit(lev_sp)

t.test(lev_sp$post, lev_sp$pre, paired = TRUE) #p = 0.013 when na.replace
                                              # p = 0.032 when na.omit
lev_sp <- mutate(lev_sp, diff = post-pre)

###
mthobs <- filter(fallobs, site %in% "MTH")

mth <- mthobs %>% group_by(timeline, checklist_dt) %>%
  summarize(Richness = n_distinct(common_name), proportion = Richness/69, 
            Shannon = diversity(observation_count, index = "shannon"), 
            Simpson = diversity(observation_count, index = "simpson"))

t.test(mth$Richness ~ mth$timeline)#0.0132, post higher
t.test(mth$Shannon ~ mth$timeline) #0.7422
t.test(mth$Simpson ~ mth$timeline) #0.3925

mthlong <- pivot_longer(mth, c(Richness, Shannon, Simpson), names_to = "measure", values_to = "values" )

mth_sum <- mthobs %>% group_by(common_name, timeline) %>%
  summarize(occurrence = n_distinct(checklist_dt),
            total = sum(observation_count), 
            avg = total/occurrence)
mth_sp <- pivot_wider(mth_sum, id_cols = common_name, names_from = timeline, 
                      values_from = avg)

mth_sp <- na.replace(mth_sp, 0)
mth_sp <- na.omit(mth_sp)

t.test(mth_sp$post, mth_sp$pre, paired = TRUE) #p = 0.056 with na.replace
                                                #p = 0.5174 with na.omit

mth_sp <- mutate(mth_sp, diff = post-pre)

