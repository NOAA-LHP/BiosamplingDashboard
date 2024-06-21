##### Prep 
library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(EnvStats)
library(mapview)
library(readxl)

jv_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/"
viz_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_visulization/"

func_groups_raw <- read_excel("//PICKINGFISH/USERS/juliette.verstaen/american_samoa_visulization/for_prep/LIST_OF_FISH_SPECIES_04-21-2020.xlsx") 

genus <- func_groups_raw %>%
  separate(TAXONNAME, c("genus", "remove"), sep = " ", remove = TRUE) %>%
  mutate(consistent_taxon = paste0(genus, " sp")) %>%
  dplyr::select(consistent_taxon, trophic = TROPHIC_DEF, lw_a = LW_A, lw_b = LW_B) %>%
  distinct() %>%
  group_by(consistent_taxon, trophic) %>% 
  summarize(lw_a = mean(lw_a),
            lw_b = mean(lw_b)) %>% 
  filter(!is.na(trophic))
  
func_groups <- func_groups_raw %>% 
  dplyr::select(consistent_taxon = TAXONNAME, trophic = TROPHIC_DEF, lw_a = LW_A, lw_b = LW_B)%>% 
  rbind(genus)



##--- TO DO
## why is the survey area for asnps 50 or 100? - two different areas depending on the pass
## viz idea - distribution plot where size is on the x and the count is on the y - have one tab that is grouped by genus or family? and then a tab where it's species that's just the "important" species

## need to add on family to the genus of the other data sets
## need to add on method
## check why some are raw_taxon and some are consistent_taxon
## add in the nearest village code



## Organize data from each organization - maybe move all this tidying into a script in the integration repo not here
## --------------------

## ASNPS

asnps_raw <- read_csv("//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/formatted_data/fish/ASNPS.csv")

asnps <- asnps_raw %>% 
  mutate(site_id = paste0(loc_type, "-",  loc_name, "-", year)) %>% 
  separate(date, c("date", "time"), sep = " ") %>% 
  separate(date, c("month", "day", "year"), sep = "/") %>% 
  mutate(month = str_pad(month, width = 2, side = "left", pad = "0")) %>% 
  mutate(day = str_pad(day, width = 2, side = "left", pad = "0")) %>% 
  mutate(date = as.Date(paste0(year, "-", month, "-", day))) %>% 
  mutate(source_description = "AS NPS",
         survey_method = "Belt transect",
         depth_strata = case_when(depth < 10 ~ "shallow",
                                  depth >=10 & depth < 20 ~ "middle",
                                  depth >=20 ~ "deep"),
         depth_strata_long = case_when(depth < 10 ~ "shallow (0-10 m)",
                                       depth >=10 & depth < 20 ~ "middle (10-20 m)",
                                       depth >=20 ~ "deep (>20 m)"),
         replicate = 1,
         biomass_g_m2 = NA,
         count_per_m2 = NA,
         notes = "size is the average length") %>% 
  dplyr::select(island, year, date, site_id, survey_method, survey_area, latitude, longitude, depth, depth_strata, depth_strata_long, replicate, consistent_taxon, genus, family, count, min, max, size_cm = avg_lgth, biomass_g_m2, count_per_m2,source_description,  notes)


## CRAG 2006-2011
crag_early_raw <- read_csv( "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/formatted_data/fish/CRAG_2006_2011.csv")

crag_early <- crag_early_raw %>% 
  mutate(survey_method = "Belt transect",
         site_id = site,
         source_description = "CRAG CORIS slopes Tutuila",
         depth_strata = case_when(depth < 10 ~ "shallow",
                                  depth >=10 & depth < 20 ~ "middle",
                                  depth >=20 ~ "deep"),
         depth_strata_long = case_when(depth < 10 ~ "shallow (0-10 m)",
                                       depth >=10 & depth < 20 ~ "middle (10-20 m)",
                                       depth >=20 ~ "deep (>20 m)"),
         min = NA,
         max = NA,
         notes = NA,
         family = NA,
         count_per_m2 = NA,
         survey_area = "30-m long with various widths for fish groups",
         date = as.Date(date)) %>% 
  dplyr::select(island, year, date, site_id, survey_method, survey_area, latitude, longitude, depth, depth_strata,depth_strata_long,  replicate, consistent_taxon, genus, family, count, min, max, size_cm,  biomass_g_m2 = biomass, count_per_m2, source_description, notes)
  

## CRAG 2015-2017
crag_late_raw <- read_csv( "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/formatted_data/fish/CRAG_2015_2017.csv") 

crag_late <- crag_late_raw %>% 
  mutate(survey_method = "SPC",
         site_id = site,
         source_description = "CRAG CORIS slopes Tutuila",
         depth_strata = case_when(depth < 10 ~ "shallow",
                                  depth >=10 & depth < 20 ~ "middle",
                                  depth >=20 ~ "deep"),
         depth_strata_long = case_when(depth < 10 ~ "shallow (0-10 m)",
                                       depth >=10 & depth < 20 ~ "middle (10-20 m)",
                                       depth >=20 ~ "deep (>20 m)"),
         min = NA,
         max = NA,
         notes = NA,
         family = NA,
         biomass_g_m2 = NA,
         count_per_m2 = NA, 
         survey_area = pi*(7.5^2),
         date = as.Date(date)) %>% 
  dplyr::select(island, year, date, site_id, survey_method, survey_area, latitude, longitude, depth, depth_strata,depth_strata_long,  replicate, consistent_taxon, genus, family, count, min, max, size_cm, biomass_g_m2, count_per_m2, source_description, notes)
  
  
## DMWR 1996-2015
dmwr_raw <- read_csv( "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/formatted_data/fish/DMWR_1996_2015.csv")


dmwr <- dmwr_raw %>% 
  mutate(site_id = ifelse(is.na(site_id), "", site_id),
         site_id = paste(site_id, site, sep = "_"),
         survey_method = "Belt transect",
         source_description = "DMWR KRSP",
         depth_strata = case_when(depth_m < 10 ~ "shallow",
                                  depth_m >=10 & depth_m < 20 ~ "middle",
                                  depth_m >=20 ~ "deep"),
         depth_strata_long = case_when(depth_m < 10 ~ "shallow (0-10 m)",
                                       depth_m >=10 & depth_m < 20 ~ "middle (10-20 m)",
                                       depth_m >=20 ~ "deep (>20 m)"),
         min = NA,
         max = NA,
         notes = "multiple replicates per survey but data came grouped",
         biomass_g_m2 = NA,
         count_per_m2 = NA, 
         replicate = 1,
         date = paste(year, month, day, sep = "-"),  
         survey_area = trans_width_m*trans_length_m,
         date = str_replace(date, "NA", "01"),
         date = as.Date(date)) %>% 
  dplyr::select(island, year, date, site_id, survey_method, survey_area, latitude, longitude, depth = depth_m, depth_strata, depth_strata_long, replicate, consistent_taxon, genus, family, count, min, max, size_cm= size, biomass_g_m2, count_per_m2, source_description, notes)  %>% 
  mutate(consistent_taxon = ifelse(consistent_taxon == "Naso tuberosus", "Naso tonganus", consistent_taxon))


## Green
green_raw <- read_csv( "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/formatted_data/fish/Green_fish.csv")

## IDK about including this cause I need to work out the biomass
 green <- green_raw %>% 
   filter(year != "1994/95") %>% 
   mutate(survey_method = "Belt transect",
          source = "Green",
          island = case_when(island == "Aunuu" ~ "Tutuila",
                             island == "Ofu-Olosega" ~ "Ofu & Olosega",
                             T ~ island),
          depth_strata = case_when(depth_m < 10 ~ "shallow",
                                   depth_m >=10 & depth_m < 20 ~ "middle",
                                   depth_m >=20 ~ "deep"),
          depth_strata_long = case_when(depth_m < 10 ~ "shallow (0-10 m)",
                                        depth_m >=10 & depth_m < 20 ~ "middle (10-20 m)",
                                        depth_m >=20 ~ "deep (>20 m)"),
          min = NA,
          max = NA,
          notes = "multiple replicates per survey but data came grouped",
          replicate = 1,
          month = "01",
          day = "01",
          size_cm = NA,
          count = NA, 
          min = NA,
          max = NA,
          date = paste(year, month, day, sep = "-"),  
          survey_area = 50*3,
          source_description = "Green") %>% 
   dplyr::select(island, year, date, site_id= site, survey_method, survey_area, latitude, longitude, depth = depth_m, depth_strata, depth_strata_long, replicate, consistent_taxon, genus, family, count, min, max, size_cm, biomass_g_m2, count_per_m2, notes, source_description)
  

## NOAA NCRMP
noaa_raw <- read_csv( "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/formatted_data/fish/NOAA_NCRMP.csv")

noaa <- noaa_raw %>% 
  mutate(survey_method = case_when(method == "belt" ~ "Belt transect",
                                   method == "SPC" ~ "SPC",
                                   method == "nSPC" ~ "nSPC"),
         site_id = site,
         source_description = "NOAA NCRMP",
         depth_strata = case_when(depth < 10 ~ "shallow",
                                  depth >=10 & depth < 20 ~ "middle",
                                  depth >=20 ~ "deep"),
         depth_strata_long = case_when(depth < 10 ~ "shallow (0-10 m)",
                                       depth >=10 & depth < 20 ~ "middle (10-20 m)",
                                       depth >=20 ~ "deep (>20 m)"),
         min = NA,
         max = NA,
         notes = NA,
         biomass_g_m2 = NA,
         count_per_m2 = NA,
         date = as.Date(date),
         survey_area = case_when(method == "belt" & size >=20 ~ 25*10,
                                 method == "belt" & size < 20 ~ 25*8,
                                 T ~ 176.7145)) %>% 
  dplyr::select(island, year, date, site_id, survey_method, survey_area, latitude, longitude, depth, depth_strata,depth_strata_long, replicate = rep, consistent_taxon = raw_taxon, genus, family, count, min, max, size_cm = size, biomass_g_m2,count_per_m2, source_description, notes) 


all_fish <- rbind(asnps, crag_early, crag_late, green, dmwr, noaa) %>% 

  mutate(source = case_when(str_detect(source_description, "DMWR") ~ "DMWR KRSP",
                            source_description == "AS NPS" ~ "AS NPS",
                            source_description == "Green" ~ "Green",
                            str_detect(source_description, "NOAA") ~ "NOAA",
                            str_detect(source_description, "CRAG") ~ "CRAG")) %>% 
  mutate(hex_number = case_when(source == "DMWR KRSP" ~ "#332288",
                              source == "AS NPS" ~ "#117733",
                              source == "NOAA" ~ "#6699CC",
                              source == "CRAG" ~ "#DDCC77",
                              source == "Green" ~ "#B71300")) %>% 
  mutate(consistent_taxon = case_when(consistent_taxon == "Cetoscarus bicolor" ~ "Cetoscarus ocellatus",
                                     consistent_taxon == "Kyphosus biggibus" ~ "Kyphosus hawaiiensis",
                                     T ~ consistent_taxon)) %>% 
  filter(consistent_taxon != "no individuals seen")


#### add in fish functional group column and some stats about that

setdiff(all_fish$consistent_taxon, func_groups$consistent_taxon)

## for CRAG surevy areas in the older betl surveys
areas_crag <- func_groups_raw %>% 
  dplyr::select(consistent_taxon = TAXONNAME, COMMONFAMILYALL) %>% 
  distinct() %>% 
  mutate(survey_area_crag_old = case_when(COMMONFAMILYALL == "Parrotfish" ~ 10*30,
                                          COMMONFAMILYALL == "Damselfish" ~ 2*30)) %>% 
  mutate(survey_area_crag_old = case_when(is.na(survey_area_crag_old) & COMMONFAMILYALL %in% c("Grouper", "Eagle Rays", "Jack", "Snapper","Grunt", "Tuna", "Emperor", "Bigeye", "Aholehole",  "Manta Ray",  "Whale","Manta" , "Barracuda" , "Hammerhead") ~ 15*30,
                                          T ~ 5*30))

func_fish <- all_fish %>% 
  left_join(func_groups, relationship ="many-to-many") %>% 
  mutate( consistent_taxon = case_when(consistent_taxon == "Chromis pacifica" ~ "Chromis agilis",
                                       T ~ consistent_taxon),
          trophic = case_when(consistent_taxon == "Pictichromis porphyrea" ~ "Mobile invertivore",
                             consistent_taxon == "Ptereleotris monoptera" ~ "Planktivore",
                             consistent_taxon == "Chromis agilis" ~ "Planktivore",
                             consistent_taxon == "Fusigobius inframaculatus" ~ "Mobile invertivore",
                             consistent_taxon == "Aphareus rutilans" ~"Piscivore",
                             consistent_taxon == "Plectropomus maculatus" ~"Piscivore",
                             consistent_taxon == "Gymnocranius euanus" ~ "Mobile invertivore",
                             consistent_taxon == "Canthigaster compressa" ~ "Herbivore",
                             consistent_taxon == "Parapercis" ~ "Mobile invertivore",
                             consistent_taxon == "Lutjanus vitta" ~ "Herbivore",
                             consistent_taxon == "Rastrelliger kanagurta" ~ "Piscivore", ## genus not in the list but it's a mackrel
                             consistent_taxon == "Halichoeres binotopsis" ~ "Mobile invertivore",
                             consistent_taxon == "Siganus fuscescens" ~ "Herbivore",
                             consistent_taxon == "Siganus lineatus" ~ "Herbivore",
                             consistent_taxon == "Sargocentron melanospilos" ~ "Mobile invertivore",
                             consistent_taxon == "Lethrinus lentjan" ~ "Mobile invertivore",
                             consistent_taxon == "Kyphosidae" ~"Herbivore",
                             consistent_taxon == "Kyphosidae sp" ~"Herbivore",
                             consistent_taxon == "Kyphosus bigibbus" ~"Herbivore",
                             consistent_taxon == "Acanthurus grammoptilus" ~ "Herbivore",
                             consistent_taxon == "Parapercis hexophtalma" ~ "Mobile invertivore",
                             T ~ trophic),
         genus = case_when(consistent_taxon == "Fusigobius inframaculatus" ~ "Fusigobius",
                           consistent_taxon == "Gymnocranius euanus" ~ "Gymnocranius",
                           T ~ genus),
         family = case_when(consistent_taxon == "Kyphosidae sp" ~ "Kyphosidae",
                            consistent_taxon == "Pictichromis porphyrea" ~ "Pseudochromidae",
                            consistent_taxon == "Fusigobius inframaculatus" ~ "Gobiidae",
                            genus == "Kyphosus" ~ "Kyphosidae",
                            )) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(source = ifelse(source == "AS NPS", "NPS AS", source)) %>% 
  mutate(depth_strata = ifelse(source == "NPS AS", "middle", depth_strata),
         depth_strata_long = ifelse(source == "NPS AS", "shallow (0-10 m)", depth_strata_long)) %>% 
  filter(consistent_taxon != "Un-id fish sp",
         consistent_taxon != "Search Results") %>%  ## can't use these
  left_join(areas_crag) %>% 
  mutate(survey_area = ifelse(source == "CRAG" & survey_method == "Belt transect", survey_area_crag_old, survey_area),
         survey_area = ifelse(consistent_taxon == "Kyphosidae sp", 5*30, survey_area)) %>% 
  dplyr::select(-COMMONFAMILYALL, -survey_area_crag_old) %>% 
  mutate(survey_area = as.numeric(survey_area))


gp_func_fish <- func_fish %>% 
  group_by(genus) %>% 
  mutate(lw_a = case_when(is.na(lw_a) ~ mean(lw_a, na.rm = TRUE),
                          T ~ lw_a),
         lw_b = case_when(is.na(lw_b) ~ mean(lw_b, na.rm = TRUE),
                          T~ lw_b)) %>% 
  ungroup() %>% 
  group_by(family) %>% 
  mutate(lw_a = case_when(is.na(lw_a) ~ mean(lw_a, na.rm = TRUE),
                          T ~ lw_a),
         lw_b = case_when(is.na(lw_b) ~ mean(lw_b, na.rm = TRUE),
                          T~ lw_b)) %>% 
  ungroup()

## look up the species level ones
## Acanthurus grammoptilus is either herbivore or planktivore - choose herbivore because the only acanthurus that is planktivore have a different mouth shape

c <- gp_func_fish %>% 
  filter(is.na(trophic)) %>% 
  dplyr::select(consistent_taxon, source) %>% 
  distinct()


## calculate the biomass and density if not provided

biomass_density_fish <- gp_func_fish %>% 
  mutate(biomass_g_m2 = ifelse(is.na(biomass_g_m2), (lw_a*(size_cm^lw_b))/survey_area, biomass_g_m2),
         count_per_m2 = ifelse(is.na(count_per_m2), count/survey_area, count_per_m2)) 
 # dplyr::select(-lw_a, -lw_b)


check <- biomass_density_fish %>% 
  filter(is.na(biomass_g_m2))

## -----------------------------
# Add in nearest village to the inventory sites
## -----------------------------

func_fish_sf <- biomass_density_fish %>% 
  filter(!is.na(latitude),
         !is.na(longitude)) %>% 
  mutate(lat = latitude,
         lon = longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%  
  rename(latitude = lat,
         longitude = lon)


tut_shape <- read_sf(paste0(jv_path, "spatial/tutuila_villages/tut_villages.shp")) %>% 
  st_transform(crs = 4326)

sites_df <- func_fish_sf %>% 
  dplyr::select(island, site_id, source_description) %>% 
  distinct() 

sites <- sites_df %>% 
  pull()

n <- length(sites)
nearest_village <- character(n)
dist_nearest_village <- numeric(n)

## For each point, find name of nearest polygon (in this case, Belgian cantons)
for (i in seq_along(nearest_village)) {
  
  g_dists <- st_distance(sites_df[i,], tut_shape)
  nearest_village[i] <- tut_shape$VILLAGE[which.min(g_dists)]
  dist_nearest_village[i] <- min(g_dists)
  
}

distance_df <- data.frame(nearest_village, dist_nearest_village)

sites_villages_sf <- cbind(sites_df, distance_df) 

sites_villages <-  sites_villages_sf %>% 
  st_drop_geometry() %>% 
  select(!dist_nearest_village) %>% 
  distinct()

villages <- sites_villages_sf %>% 
  dplyr::select(nearest_village) %>% 
  distinct()

order_villages <- sort(unique(villages$nearest_village))

biomass_density_fish <- func_fish_sf %>% 
  st_join(villages) 

## fix the non tutuila villages

biomass_density_fish <- biomass_density_fish %>% 
  mutate(nearest_village = case_when(island == "Swains" ~ "Swains",
                                     island == "Ofu & Olosega" ~ "Ofu & Olosega",
                                     island == "Rose" ~ "Rose",
                                     island == "South Bank" ~ "South Bank",
                                     island == "Tau" ~ "Tau",
                                     T ~ nearest_village),
         source_description = case_when(source_description == "NOAA NCRMP" ~ "NOAA",
                                        source_description == "CRAG CORIS slopes Tutuila" ~ "CRAG",
                                        source_description == "DMWR KRSP" ~ "DMWR",
                                        source_description == "AS NPS" ~ "NPAS",
                                        source_description == "Green" ~ "Green"))


write_csv(biomass_density_fish, paste0(viz_path, "prep/fish_site.csv"))


## summarize biomass and density at the village scale

## bring in the centroid point file for the village ocean level shapefile - if not saved with centroid bring in the shapefile and then add centroid

villages_ocean_tut <- read_sf(paste0(jv_path, "spatial/tutuila_villages_ocean/tutuila_villages_ocean.shp")) %>% 
  st_transform(crs = 4326) %>% 
  dplyr::select(nearest_village = nrst_vl, longitude = CenLon, latitude = CenLat) %>% 
  st_drop_geometry()


other_islands <- data.frame(nearest_village = c("Ofu & Olosega", "Swains", "Rose", "South Bank", "Tau"),
                               latitude = c(-14.170772, -11.054817, -14.545894, -14.87711, -14.233874),
                               longitude = c(-169.640894, -171.078422,-168.156831, -170.6325, -169.464736)) 

villages_centroids <- rbind(villages_ocean_tut, other_islands) 


fish_village <- biomass_density_fish %>% 
  st_drop_geometry()%>% 
  group_by(island, year, nearest_village, depth_strata, depth_strata_long, consistent_taxon, genus, family, trophic, source_description) %>%
  summarize(biomass_g_m2_village_mean = mean(biomass_g_m2),
            count_per_m2_village_mean = mean(count_per_m2)) %>% 
  ungroup() %>% 
  left_join(villages_centroids)
 # st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

t_20_bio <- fish_village %>% 
  group_by(consistent_taxon) %>% 
  filter(!(genus %in% c("Aetobatus", "Carcharhinus", "Gymnothorax"))) %>% 
  summarize(total_m_biomass = mean(biomass_g_m2_village_mean)) %>% 
  ungroup() %>% 
  arrange(-total_m_biomass) %>% 
  head(20) %>% 
  pull(consistent_taxon)

t_20_den <- fish_village %>% 
  group_by(consistent_taxon) %>% 
  filter(!(genus %in% c("Aetobatus", "Carcharhinus", "Gymnothorax"))) %>% 
  summarize(total_m_density = mean(count_per_m2_village_mean)) %>% 
  ungroup() %>% 
  arrange(-total_m_density) %>% 
  head(20) %>% 
  pull(consistent_taxon)

fish_village <- fish_village %>% 
  mutate(top_20_biomass = ifelse(consistent_taxon %in% t_20_bio, "yes", "no"),
         top_20_density = ifelse(consistent_taxon %in% t_20_den, "yes", "no"))

#mapview(filter(fish_village, top_20_biomass == "yes" ), zcol = "biomass_g_m2_village_mean")

write_csv(fish_village, paste0(viz_path, "prep/fish_village_biomass_density.csv"))

## do summary df at the island level - no need for spatial aspect
fish_island <- biomass_density_fish %>% 
  group_by(island, year,depth_strata, depth_strata_long, consistent_taxon, genus, family, trophic, source_description) %>%
  summarize(biomass_g_m2_village_mean = mean(biomass_g_m2),
            count_per_m2_village_mean = mean(count_per_m2)) %>% 
  ungroup()

write_csv(fish_island, paste0(viz_path, "prep/fish_island_biomass_density.csv"))








# 
# 
# 
# #REMOVE?
# ## summarize counts at the survey level - average between replicates
# 
# fish_counts <- func_fish %>% 
#   group_by(island, year, date, site_id, survey_method, survey_area, latitude, longitude, depth, depth_strata, depth_strata_long, consistent_taxon, genus, source_description, source, replicate, trophic, nearest_village) %>% 
#   summarize(total_rep_count = sum(count, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   group_by(island, year, date, site_id, survey_method, survey_area, latitude, longitude, depth, depth_strata, depth_strata_long, consistent_taxon, genus, source_description, source, trophic, nearest_village) %>% 
#   summarize(mean_count = mean(total_rep_count, na.rm = TRUE)) %>% 
#   ungroup() 
# 
#  ## check that we have all the species of interest
# species_oi <- c("Siganus spinus", "Kyphosus cinerascens", "Scarus schlegeli", "Caranx melampygus", "Epinepheuls merra", "Naso literatus", "Naso unicornis", "Acanthurus triostegus", "Acanthurus lineatus", "Monotoxis grandoculus", "Lutjanus fulvus", "Lethrinus harak", "Lethrinus obseletus", "Letrhinus olivaceus", "Cheilinus undulatus", "Cheilinus trilobatus", "Scarus frontalis", "Hipposcarus longiceps", "Lethrinus rubrioperculatus", "Epinephus faciatus", "Acanthurus xanthochelus", "Scarus sordidus")
# 
# setdiff(species_oi, fish_counts$consistent_taxon)
# write_csv(fish_counts, paste0(viz_path, "prep/fish_count.csv"))
# 
# ## summarized at the functional level and add in the total transects for the top of the bars
#  
# ## do a column with total count of surveys
# 
# func_fish_count <- fish_counts %>%
#   group_by(island, year, depth_strata_long, trophic) %>% 
#   summarize(sum_count = sum(mean_count)) %>% 
#   ungroup() %>% 
#   group_by(island, year, depth_strata_long) %>% 
#   mutate(island_year_total_fish = sum(sum_count)) %>% 
#   ungroup() %>% 
#   mutate(proportion = sum_count/island_year_total_fish,
#          proportion_round = round(proportion, digits = 2)) %>% 
#   st_drop_geometry()%>% 
#   filter(island != "South Bank")
# 
# total_surveys_island <- fish_counts %>% 
#   st_drop_geometry()%>%
#   mutate(unique_site = paste0(date, "_", site_id)) %>% 
#   dplyr::select(island, year, depth_strata_long, unique_site) %>% 
#   distinct() %>% 
#   group_by(island, year, depth_strata_long) %>% 
#   count() %>% 
#   ungroup() %>% 
#   dplyr::select(island, year, depth_strata_long, surveys_total = n)
# 
# 
# func_fish_count <- left_join(func_fish_count, total_surveys_island)
# 
# 
# fish_island_summarized_labs <- func_fish %>% 
#   dplyr::select(year, island, site_id, date, depth_strata_long ) %>% 
#   distinct() %>% 
#   group_by(year, island, depth_strata_long) %>% 
#   summarize(total_n = n()) %>% 
#   mutate(proportion = 1) %>% 
#   ungroup()
# 
# write_csv(func_fish_count, paste0(viz_path, "prep/fish_island_functional.csv"))
# write_csv(fish_island_summarized_labs, paste0(viz_path, "prep/fish_island_summarized_labs.csv"))
# 
