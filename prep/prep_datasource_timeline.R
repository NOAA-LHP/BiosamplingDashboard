#########################
#### Prep Water quality data
########################

## -----------------------------
## Load libraries and paths
## -----------------------------

library(tidyverse)
data_inventory                       <- read_csv(paste0(viz_path, "prep/data_inventory.csv"))
viz_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_visulization/"

datasource_timeline_df <- data_inventory %>% 
  dplyr::select(island, year, survey_method, data_type, source) %>% 
  mutate(source = case_when(source == "CRAG photolevel points" ~ "CRAG Longterm Tutuila Monitoring",
                            source == "NOAA - Whital" ~ "NOAA - Whitall",
                            source == "Ridge to Reef (CRAG|AS-EPA|NMSAS)" ~ "CRAG", 
                            T ~ source)) %>% 
  distinct() %>% 
  filter(island != "South Bank") %>% 
  mutate(time_period = ifelse(year <2000, "Pre-2000", "Post-2000"))

write_csv(datasource_timeline_df, paste0(viz_path, "prep/datasource_timeline.csv"))

# 
# ggplot(datasource_timeline_df) +
#   geom_point(aes(x = year, y = source, color = data_type), position = position_dodge(0.8)) +
#   facet_wrap(vars(island), ncol = 1)
# 
# 




