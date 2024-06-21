
#########################
#### Prep Events Timeline
########################

## -----------------------------
## Load libraries and paths
## -----------------------------

library(tidyverse)

viz_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_visulization/"

## https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/

## -----------------------------
## Benthic Cover Inventory Sites
## -----------------------------

# category_levels <- c("biophysical", "governance", "socio-economic")
# category_colors <- c("#00797F", "#003087", "#DB6015")


events_df <- read_csv(paste0(viz_path, "for_prep/events_timeline.csv")) %>%
  filter(include == "x") %>%
  mutate(placeholder = 0.1,
         event_spaces = paste(" ", event), 
         time_period = ifelse(start_year <2000, "Pre-2000", "Post-2000"),
         event_long = paste0(start_year, "-", end_year, ": ", event),
         event_long = str_replace(event_long, "-NA", ""),
         date = ymd(paste0(start_year, "01", "01", sep = "-")),
       #  category = factor(category, levels = category_levels, order = category_colors)
         ) %>% 
  group_by(date) 

positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions <- c(1, -1)
text_offset <- 0.08

line_pos <- data.frame(
  "date"=unique(events_df$date),
  "position"=rep(positions, length.out=length(unique(events_df$date))),
  "direction"=rep(directions, length.out=length(unique(events_df$date))))

events_df_pos <- events_df %>% 
  left_join(line_pos) %>% 
  group_by(date) %>% 
  mutate(month_count = 1:n(),
         text_position = (month_count*text_offset*direction)+ position)

write_csv(events_df_pos, paste0(viz_path, "prep/events_timeline.csv"))

