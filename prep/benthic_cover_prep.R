##### Prep 
library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(EnvStats)
library(mapview)

jv_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/"
viz_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_visulization/"

## Benthic cover by group

## --------------------

# colors <- carto_pal(12, "Safe") 
# show_col(colors)

benthic_cover_raw <- read_csv("//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/integrated_data/benthic_cover_group.csv") 
noaa_depth_bins <- read_csv(paste(jv_path, "raw_data/biological/NOAA_ESD/Benthic/oracle_download/noaa_benthic_surveys.csv", sep = "")) %>% 
  dplyr::select(site_name = SITE, year = OBS_YEAR, noaa_bin = DEPTH_CODE) %>% 
  distinct() %>% 
  mutate(year = as.character(year))

## PICK UP HERE
number_transects <- benthic_cover_raw %>% 
  dplyr::select(island, site_name, date, year, transect, source_description) %>% 
  distinct() %>% 
  group_by(island, site_name, date, year) %>% 
  mutate(n_transects = n())


benthic_cover_site_recalc <- benthic_cover_raw %>% 
  filter(year != "2008/2009" ) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
  mutate(group = ifelse(group %in% c("Cyanobacteria", "Chrysophyte"), "Other", group)) %>% 
  group_by(island, site_name, date, month, day, year, depth, transect, survey_method, survey_area,source_description, reef_type, group, site_name_structure, data_id, nearest_village, latitude, longitude) %>% 
  summarize(p_cover = sum(p_cover)) %>% 
  ungroup() %>% 
  left_join(number_transects) %>% 
  group_by(island, site_name, date, month, day, year, depth, survey_method, source_description, survey_area, reef_type, site_name_structure, data_id, nearest_village, latitude, longitude, n_transects, group) %>% 
  summarize(means_sum = sum(p_cover)) %>% 
  mutate(percent_cover = means_sum/n_transects) %>% 
  ungroup()

# ch <- test %>%
#   group_by(island, site_name, date) %>%
#   summarize(mean = sum(means_sum))

benthic_cover_group <- benthic_cover_site_recalc %>% 
  mutate(island = case_when(site_name %in% c("Fagaitua Bay", "Matautele Pt.", "Siliaga Pt.") ~ "Tutuila",
                            T ~ island),
         
         ## make sure everything has a depth bin
         depth_strata_short = case_when(depth < 10 ~ "shallow",
                                  depth >=10 & depth < 20 ~ "middle",
                                  depth >=20 ~ "deep"),
         depth_strata_long = case_when(depth < 10 ~ "shallow (0-10 m)",
                                  depth >=10 & depth < 20 ~ "middle (10-20 m)",
                                  depth >=20 ~ "deep (>20 m)")) %>% 
  mutate(depth_strata_short = ifelse(source_description %in% c("NOAA LPI", "Ridge 2 Reef"), "middle", depth_strata_short),
         depth_strata_long = ifelse(source_description %in% c("NOAA LPI", "Ridge 2 Reef"), "middle (10-20 m)", depth_strata_long)) %>% 
  left_join(noaa_depth_bins) %>% 
  mutate(depth_strata_short = case_when(noaa_bin == "S" ~ "shallow",
                                        noaa_bin == "M" ~ "middle",
                                        noaa_bin == "D" ~ "deep",
                                        T ~ depth_strata_short),
         depth_strata_long = case_when(noaa_bin == "S" ~ "shallow (0-10 m)",
                                       noaa_bin == "M" ~  "middle (10-20 m)",
                                       noaa_bin == "D"~ "deep (>20 m)",
                                       T ~ depth_strata_long)) %>% 
  mutate(percent_cover = percent_cover*100) %>% 
  mutate(percent_cover = round(percent_cover, 2)) %>% 
  mutate(source = case_when(source_description == "DMWR KRSP slopes Tutuila" ~ "DMWR KRSP",
                            source_description == "ASNPS" ~ "AS NPS",
                            str_detect(source_description, "NOAA") ~ "NOAA",
                            str_detect(source_description, "CRAG") ~ "CRAG",
                            source_description == "Green et al 2020" ~ "Green et al 2020",
                            source_description == "Ridge 2 Reef" ~ "CRAG | AS-EPA | NMSAS")) %>% 
  mutate(hex_number = case_when(source == "DMWR KRSP" ~ "#332288",
                              source == "AS NPS" ~ "#117733",
                              source == "NOAA" ~ "#6699CC",
                              source == "CRAG" ~ "#DDCC77",
                              source == "Green et al 2020" ~ "#CC6677",
                              source == "CRAG | AS-EPA | NMSAS" ~ "#882255")) %>% 
  mutate(survey_method = case_when(survey_method == "Quarter Point Intercept (QPI)" ~ "quarter point intercept (QPI)",
                                   survey_method == "Line Point Intercept (LPI)" ~ "line point intercept (LPI)",
                                   survey_method == "Photoquad (CPCe on video stills)" ~ "photoquad (CPCe on video stills)",
                                   survey_method == "Photoquad (CPCe)" ~ "photoquad (CPCe)",
                                   survey_method == "Photoquad (CNET)" ~ "photoquad (CNET)",
                                   T ~ survey_method)) %>% 
  mutate(hex_number_group = case_when(group == "CCA" ~ "#CC79A7",
                                      group == "Hard coral" ~ "#E69F00",
                                      group == "Macroalgae" ~ "#0072B2",
                                      group == "Other" ~ "#D55E00",
                                      group == "Substrate" ~ "#F0E442",
                                      group == "Turf" ~ "#009E73",
                                      group == "Encrusting macroalgae" ~ "#56B4E9")) %>% 
  mutate(percent_cover_legend = case_when(percent_cover <20 ~ "0-20%",
                                          percent_cover >=20 & percent_cover<40 ~ "20-40%",
                                          percent_cover >=40 & percent_cover<60 ~ "40-60%",
                                          percent_cover >=60 & percent_cover<80 ~ "60-80%",
                                          percent_cover >=80 & percent_cover<100 ~ "80-100%"))


ch <- benthic_cover_group %>%
  group_by(island, site_name, date, depth_strata_long) %>%
  summarize(mean = sum(percent_cover))

write_csv(benthic_cover_group, paste0(viz_path, "prep/benthic_cover_group.csv"))


# ## Benthic cover by group summarized at the island, year, depth level

twice_annual <- benthic_cover_group %>%
  filter(year != "2008/2009" ) %>%
  dplyr::select(island, year, date, site_name, depth_strata_long) %>%
  distinct() %>%
  group_by(island, year, site_name, depth_strata_long) %>%
  mutate(twice_annual = n()) %>%
  filter(twice_annual == 2) %>%
  st_drop_geometry()

fix <- left_join(twice_annual, benthic_cover_group) %>%
  arrange(island, year, site_name, date) %>%
  dplyr::select(island, year, date, site_name, depth_strata_long) %>%
  distinct() %>%
  mutate(extra = rep(c(1,2), length.out = n()),
        site_name_new = paste(site_name, extra, sep = "-"))%>%
  st_drop_geometry()

annual_surveys <- benthic_cover_group %>%
  filter(year != "2008/2009" ) %>%
  left_join(fix) %>%
  mutate(site_name = ifelse(!is.na(site_name_new), site_name_new, site_name)) %>%
  group_by(island, year, site_name, depth_strata_long) %>%
  dplyr::select(island, year, site_name, depth_strata_long, depth_strata_short) %>%
  distinct() %>%
  group_by(island, year, depth_strata_long) %>%
  summarize(total_surveys = n(),
            total_surveys_cover =total_surveys*100) %>%
  ungroup()%>%
  st_drop_geometry()

benthic_island_summarized <- benthic_cover_group %>%
  st_drop_geometry() %>% 
  filter(year != "2008/2009" ) %>%
  left_join(fix) %>%
  mutate(site_name = ifelse(!is.na(site_name_new), site_name_new, site_name)) %>%
  dplyr::select(island, site_name, date, year, hex_number_group, reef_type, group, percent_cover, depth_strata_short, depth_strata_long) %>%
  left_join(annual_surveys) %>%
  group_by(island, year, group,hex_number_group, depth_strata_long, total_surveys) %>%
  summarize(cover_island = sum(percent_cover)) %>%
  ungroup() %>%
  mutate(island_p_cover = (cover_island)/total_surveys) %>%
  mutate(island_p_cover_round = round(island_p_cover, digits = 2))

# test <- benthic_island_summarized %>%
#   group_by(island, year, depth_strata_long) %>%
#   summarize(mean_island = sum(island_p_cover_round/100)) %>%
#   ungroup()

write_csv(benthic_island_summarized, paste0(viz_path, "prep/benthic_island_summarized.csv"))   

benthic_island_summarized_labs <- benthic_island_summarized %>% 
  dplyr::select(year, island, depth_strata_long, n_surveys= total_surveys ) %>% 
  mutate(island_p_cover = 1)

write_csv(benthic_island_summarized_labs, paste0(viz_path, "prep/benthic_island_summarized_labs.csv"))   

## Do the same thing but at the village level

annual_surveys_village <- benthic_cover_group %>%
  filter(year != "2008/2009" ) %>%
  left_join(fix) %>%
  mutate(site_name = ifelse(!is.na(site_name_new), site_name_new, site_name)) %>%
  group_by(island, year, site_name, nearest_village, depth_strata_long) %>%
  dplyr::select(island, year, site_name, nearest_village, depth_strata_long, depth_strata_short) %>%
  distinct() %>%
  group_by(island, year, nearest_village, depth_strata_long) %>%
  summarize(total_surveys = n()) %>%
  ungroup()%>%
  st_drop_geometry()

benthic_island_village_summarized <- benthic_cover_group %>%
  filter(year != "2008/2009" ) %>%
  left_join(fix) %>%
  mutate(site_name = ifelse(!is.na(site_name_new), site_name_new, site_name)) %>%
  dplyr::select(island, site_name, date, year, nearest_village, hex_number_group, reef_type, group, percent_cover, depth_strata_short, depth_strata_long) %>%
  left_join(annual_surveys_village) %>%
  group_by(island, year, group, nearest_village, depth_strata_long, total_surveys, hex_number_group) %>%
  summarize(cover_village = sum(percent_cover)) %>%
  ungroup() %>%
  mutate(island_p_cover = (cover_village)/total_surveys) %>%
  ungroup() %>%
  mutate(island_p_cover_round = round(island_p_cover, digits = 2))

test <- benthic_island_village_summarized %>%
  group_by(island, year, nearest_village, depth_strata_long) %>%
  summarize(mean_village = sum(island_p_cover_round/100)) %>%
  ungroup()

write_csv(benthic_island_village_summarized, paste0(viz_path, "prep/benthic_cover_village.csv"))   
