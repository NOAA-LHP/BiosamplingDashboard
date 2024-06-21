#########################
#### Prep Data Inventory 
########################

## -----------------------------
## Load libraries and paths
## -----------------------------

library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(EnvStats)
library(mapview)

jv_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/"
viz_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_visulization/"

## -----------------------------
## Benthic Cover Inventory Sites
## -----------------------------

benthic_inventory <- read_csv(paste0(jv_path, "integrated_data/benthic_cover_group.csv"))%>% 
  mutate(data_type = "benthic cover") %>% 
  dplyr::select(island, site_name, year, depth, survey_method, reef_type, latitude, longitude, data_type, source = source_description) %>% 
  distinct() %>% 
  filter(year != "2008/2009") %>% 
  mutate(survey_method = case_when(survey_method %in% c("Photoquad (CPCe on video stills)",
                                                        "Photoquad (CPCe)",
                                                        "Photoquadrats (CPCe)",
                                                        "Photoquad (CNET)") ~ "photoquadrat",
                                   T ~ survey_method)) %>% 
  filter(!str_detect(site_name, "2013_Fixed_OFU"))

## -----------------------------
# Coral Demography Inventory Sites
## -----------------------------

aua_hist_dem_sites <-  read_csv(paste0(jv_path, "formatted_data/biodiversity/1917_2019_aua_historical.csv")) %>% 
  mutate(source = "Aua Historical Transect",
         data_type = "coral demography",
         survey_method = "field quadrat") %>% 
  dplyr::select(island, site_name, year, depth, survey_method, reef_type, latitude, longitude, data_type, source) %>% 
  distinct()

## some extra prep since this isn't really being integrated I need to do some clean up
## --------------------

## Aua sites

aua_dem_sites <- read_csv(paste0(jv_path, "formatted_data/any_metric/2022_NOAA_Aua.csv")) %>% 
  dplyr::select(island, site_name, date, year, depth, reef_type, source_description,  latitude, longitude) %>% 
  distinct() %>% 
  mutate(total_area_m2 = case_when(site_name == "TUT-6058" ~ "7.5",
                                   T ~ "10"),
         survey_method = "REA segments",
         data_type = "coral demography",
         reef_type = tolower(reef_type),
         source = "NOAA - Smith") %>% 
  dplyr::select(island, site_name, year, depth, survey_method, reef_type, latitude, longitude, data_type, source) 


## CRAG sites
crag_demo_sites <- read_csv(paste0(jv_path, "raw_data/biological/CRAG/ASCRMP_tutuila/demography/2021_metadata.csv")) %>% 
  separate(date, c("month", "day", "year"), sep = "/") %>% 
  mutate(island = "Tutuila",
         reef_type = "forereef",
         date = paste0(year, "-", month, "-", day),
         survey_method = "field quadrat",
         data_type = "coral demography",
         total_area_m2 = area_m2*total_reps,
         depth = 10,
         source_description = "CRAG Longterm Tutuila Monitoring") %>% 
  mutate(source = "CRAG Longterm Tutuila Monitoring")%>% 
  dplyr::select(island, site_name, year, depth, survey_method, reef_type, latitude, longitude, data_type, source) 

## R2R sites - same as 2016 CRAG
mc_coords <- read_csv(paste0(jv_path, "raw_data/biological/CRAG/ASCRMP_tutuila/micronesia_challenge_download/CRAG_benthic_coral_cover_data.csv")) %>% 
  dplyr::select(site_name = local_name, x, y) %>% 
  distinct() 

r2r_demo_sites <- read_csv(paste0(jv_path, "raw_data/biological/Ridge2Reef/coral_demography/R2R_lookup_metadata.csv")) %>% 
  mutate(total_area_m2 = `total # of Quads`*`Quad size (m2)`) %>% 
  separate(Date, c("month", "day", "year"), sep = "/") %>% 
  mutate(site_name = case_when(`Site name` == "Fagasa 1" ~ "Fagasa",
                               `Site name` ==  "Fagasa 2" ~ "Fagasa_2 (ASCRMP))",
                               `Site name` == "Leone 1" ~ "Leone",
                               `Site name` == "Leone 2" ~ "Leone_2 (ASCRMP))",
                               `Site name` == "Aasu" ~ "Aasu/ Massacre",
                               T ~ `Site name`)) %>% 
  mutate(site_name = str_replace(site_name, "'", "")) %>% 
  mutate(reef_type = "forereef",
         survey_method = "field quadrat",
         date = paste0(year, "-", month, "-", day),
         data_type = "coral demography",
         depth = 10,
         source_description = "CRAG Tutuila long term monitoring",
  ) %>% 
  left_join(mc_coords) %>% 
  mutate(x = case_when(site_name == "Fagasa_2 (ASCRMP))" ~ -14.28381,
                       site_name == "Leone_2 (ASCRMP))" ~ -14.34256,
                       site_name == "Nua-Seetaga" ~ -14.32890,
                       site_name == "Sita" ~ 	-14.28945,
                       T ~ x),
         y = case_when(site_name == "Fagasa_2 (ASCRMP))" ~ -170.7621,
                       site_name == "Leone_2 (ASCRMP))" ~ -170.7887,
                       site_name == "Nua-Seetaga" ~ -170.8084,
                       site_name == "Sita" ~ -170.7400,
                       T ~ y)) %>% 
  mutate(source = "CRAG Longterm Tutuila Monitoring") %>% 
  dplyr::select(island = Island, site_name, year, depth, survey_method, reef_type, latitude = x, longitude =y, data_type, source) 

## NCRMP

ncrmp_demo_sites<- read_csv(paste0(jv_path, "raw_data/biological/NOAA_ESD/Benthic/BenthicREA_sitedata_GENUS.csv")) %>%
  filter(ISLAND %in%  c("Tutuila", "Ofu & Olosega", "Rose", "Swains", "Tau", "South Bank")) %>% 
  mutate(depth = (MIN_DEPTH_M + MAX_DEPTH_M)/2,
         reef_type = tolower(REEF_ZONE),
         survey_method = "REA segments",
         data_type = "coral demography",
         source = "NOAA - NCRMP",
         source_description = "NOAA NCRMP REA segements",
         total_area_m2 = NA) %>% 
  dplyr::select(island= ISLAND, site_name = SITE, year = OBS_YEAR, depth, survey_method, reef_type, latitude = LATITUDE, longitude = LONGITUDE, data_type, source) 


demo_inventory  <- rbind(aua_hist_dem_sites, aua_dem_sites, crag_demo_sites, r2r_demo_sites, ncrmp_demo_sites)


## -----------------------------
# Fish Inventory Sites
## -----------------------------

## ASNPS
asnps_fish <-  read_csv(paste0(jv_path, "formatted_data/fish/ASNPS.csv")) %>% 
  mutate(source = "ASNPS",
         data_type = "fish counts",
         site_name = paste0(year, "_", loc_type, "_", loc_name),
         survey_method = "belt transect",
         reef_type = "slope") %>% 
  dplyr::select(island, site_name, year, depth, survey_method, reef_type, latitude, longitude, data_type, source) %>% 
  distinct()

## CRAG

crag_fish_1 <-  read_csv(paste0(jv_path, "formatted_data/fish/CRAG_2006_2011.csv")) %>% 
  mutate(source = "CRAG",
         data_type = "fish counts",
         survey_method = "belt transect",
         reef_type = "slope") %>% 
  dplyr::select(island, site_name = site, year, depth, survey_method, reef_type, latitude, longitude, data_type, source) %>% 
  distinct()

crag_fish_2 <-  read_csv(paste0(jv_path, "formatted_data/fish/CRAG_2015_2017.csv")) %>% 
  mutate(source = "CRAG",
         data_type = "fish counts",
         survey_method = "SPC",
         reef_type = "slope") %>% 
  dplyr::select(island, site_name = site, year, depth, survey_method, reef_type, latitude, longitude, data_type, source) %>% 
  distinct()

### DMWR
dmwr_fish <-  read_csv(paste0(jv_path, "formatted_data/fish/DMWR_1996_2015.csv")) %>% 
  mutate(source = "DMWR",
         data_type = "fish counts",
         survey_method = "belt transect",
         reef_type = "slope",
         depth = NA) %>% 
  dplyr::select(island, site_name = site_id, year, depth, survey_method, reef_type, latitude, longitude, data_type, source) %>% 
  distinct()

### Green et al
green_fish <-  read_csv(paste0(jv_path, "formatted_data/fish/Green_fish.csv")) %>% 
  mutate(source = "Green et al.",
         data_type = "fish counts",
         survey_method = "belt transect") %>% 
  dplyr::select(island, site_name = site, year, depth = depth_m, survey_method, reef_type = habitat, latitude, longitude, data_type, source) %>% 
  distinct()

### NOAA
noaa_fish <-  read_csv(paste0(jv_path, "formatted_data/fish/NOAA_NCRMP.csv")) %>% 
  mutate(source = "NOAA - NCRMP",
         data_type = "fish counts",
         ifelse(method == "belt", "belt transect", method)) %>% 
  dplyr::select(island, site_name = site, year, depth, survey_method = method, reef_type = reefzone, latitude, longitude, data_type, source) %>% 
  distinct()


fish_inventory  <- rbind(asnps_fish, crag_fish_1, crag_fish_2, dmwr_fish, green_fish, noaa_fish)


## -----------------------------
# Temperature Sites
## -----------------------------

temperature_inventory <- read_csv(paste0(viz_path, "prep/insitu_temperature.csv")) %>% 
  mutate(survey_method = case_when(source == "CRAG" ~ "temperature logger",
                                   source == "NOAA - NCRMP" ~ "NCRMP StR",
                                   source == "NOAA - Smith" ~ "CTD cast"),
         data_type = "in situ temperature") %>% 
  dplyr::select(island, site_name = site_id, year, depth, survey_method, reef_type,  latitude, longitude, data_type, source) 

## -----------------------------
# Water Quality Sites
## -----------------------------

### AS EPA
asepa_wq <-  read_csv(paste0(jv_path, "formatted_data/environmental/asepa_wq.csv")) %>% 
  mutate(source = "AS-EPA",
         data_type = "in situ water quality",
         island = "Tutuila",
         method = "field Sampling",
         depth = NA,
         reef_type = "reef flat") %>% 
  dplyr::select(island, site_name = site, year, depth, survey_method = method, reef_type,  latitude, longitude, data_type, source) %>% 
  distinct()

## CRAG/R2R
r2r_wq <-  read_csv(paste0(jv_path, "formatted_data/environmental/R2R_wq.csv")) %>% 
  separate(date, c("year", "month", "day"), sep = "-") %>% 
  mutate(source = "CRAG | AS-EPA | NMSAS",
         data_type = "in situ water quality",
         island = "Tutuila",
         method = "field Sampling",
         depth = NA,
         reef_type = location) %>% 
  dplyr::select(island, site_name = site, year, depth, survey_method = method, reef_type,  latitude, longitude, data_type, source) %>% 
  distinct()

## NOAA/Whitall - Fagatele
noaa_fagatele_wq <-  read_csv(paste0(jv_path, "formatted_data/environmental/noaa_whitall_fagatele.csv")) %>% 
  mutate(source = "NOAA - Whitall",
         data_type = "in situ water quality",
         island = "Tutuila",
         method = "field Sampling",
         depth = NA, 
         year = 2016,
         reef_type = NA) %>% 
  dplyr::select(island, site_name = site_id, year, depth, survey_method = method, reef_type,  latitude, longitude, data_type, source) %>% 
  distinct()

## NOAA/Whitall - Vatia
noaa_vatia_wq <-  read_csv(paste0(jv_path, "formatted_data/environmental/noaa_whitall_vatia.csv")) %>% 
  separate(start_date, c("year", "month", "day"), sep = "-", remove = FALSE) %>% 
  mutate(source = "NOAA - Whitall",
         data_type = "in situ water quality",
         island = "Tutuila",
         method = "field Sampling",
         depth = NA, 
         reef_type = NA) %>% 
  dplyr::select(island, site_name = site_id, year, depth, survey_method = method, reef_type,  latitude, longitude, data_type, source) %>% 
  distinct()

## NOAA/Smith - Aua
noaa_aua_wq <-  read_csv(paste0(jv_path, "formatted_data/environmental/noaa_smith_aua.csv")) %>% 
  separate(start_date, c("year", "month", "day"), sep = "-", remove = FALSE) %>% 
  mutate(island = "Tutuila",
         data_type = "in situ water quality",
         method = "field Sampling",
         depth = NA, 
         reef_type = NA,
         source = "NOAA - Smith") %>% 
  dplyr::select(island, site_name = site_id, year, depth, survey_method = method, reef_type,  latitude, longitude, data_type, source) %>% 
  distinct()

wq_inventory <- rbind(asepa_wq, r2r_wq, noaa_fagatele_wq, noaa_vatia_wq, noaa_aua_wq)

## -----------------------------
# Combine all together into one df and make some tidy changes
## -----------------------------

data_inventory <- rbind(benthic_inventory, demo_inventory, fish_inventory, temperature_inventory, wq_inventory)  %>% 
  mutate(island = case_when(island == "Aunuu" ~ "Tutuila",
                            island == "Olosega" ~ "Ofu & Olosega",
                            island == "Ofu" ~ "Ofu & Olosega",
                            island == "Ofu-Olosega" ~ "Ofu & Olosega",
                            T ~ island),
         year = ifelse(year == "1994/95", "1994", year),
         year = as.numeric(year),
         source = ifelse(source == "Green et al", "Green et al.", source))


data_inventory_sf <- data_inventory %>% 
  filter(!is.na(latitude),
         !is.na(longitude)) %>% 
  mutate(lat = latitude,
         lon = longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%  
  rename(latitude = lat,
         longitude = lon)


## -----------------------------
# Add in nearest village to the inventory sites
## -----------------------------

tut_shape <- read_sf(paste0(jv_path, "spatial/tutuila_villages/tut_villages.shp")) %>% 
  st_transform(crs = 4326)

sites_df <- data_inventory_sf %>% 
  dplyr::select(island, site_name, source) %>% 
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

data_inventory_int <- data_inventory_sf %>% 
  st_join(villages) 

#mapview(data_inventory, zcol = "nearest_village") +
#  mapview(tut_shape)


## last minute source name fixes

data_inventory <- data_inventory_int %>% 
  mutate(source = case_when(source == "DMWR KRSP slopes Tutuila" ~ "DMWR KRSP",
                            source == "DMWR" ~ "DMWR KRSP",
                            source == "NOAA LPI" ~ "NOAA - NCRMP",
                            source == "NOAA photoquads" ~ "NOAA - NCRMP",
                            source == "CRAG CORIS slopes Tutuila data 2005-2015" ~ "CRAG Longterm Tutuila Monitoring",
                            source == "CRAG LPI flats from Fenner 2007-2012"  ~ "CRAG Longterm Tutuila Monitoring",
                            source == "CRAG CPCE slopes Tutuila data 2018-2020"  ~ "CRAG Longterm Tutuila Monitoring",
                            source == "CRAG"  ~ "CRAG Longterm Tutuila Monitoring",
                            source == "CRAG LPI Manua pools 2015"  ~ "CRAG Longterm Manu'a Monitoring",
                            source == "CRAG LPI Manua pools 2017-2019"  ~ "CRAG Longterm Manu'a Monitoring",
                            source == "Green et al 2020"  ~ "Green et al.",
                            T ~ source)) %>% 
  mutate(island = case_when(site_name %in% c("Fagaitua Bay", "Matautele Pt.", "Siliaga Pt.") ~ "Tutuila",
                            T ~ island)) %>% 
  mutate(source = ifelse(source == "NOAA Aua LBSP", "NOAA - Smith", source),
         source = ifelse(source == "Ridge 2 Reef", "CRAG | AS-EPA | NMSAS", source),
         survey_method = ifelse(survey_method == "Photoquadrats (CPCe)", "Photoquad (CPCe)", survey_method),
         reef_type = tolower(reef_type),
         reef_type = case_when(reef_type == "flat" ~ "reef flat",
                             reef_type == "protected slope" ~ "slope",
                             T ~ reef_type),
         survey_method = case_when(survey_method == "Quarter Point Intercept (QPI)" ~ "quarter point intercept (QPI)",
                                   survey_method == "Line Point Intercept (LPI)" ~ "line point intercept (LPI)",
                                   survey_method == "Photoquad (CPCe on video stills)" ~ "photoquad (CPCe on video stills)",
                                   survey_method == "Photoquad (CPCe)" ~ "photoquad (CPCe)",
                                   survey_method == "Photoquad (CNET)" ~ "photoquad (CNET)",
                                   survey_method == "field Sampling" ~ "field sampling",
                                   survey_method == "belt" ~ "belt transect",
                                   T ~ survey_method)) %>% 
  mutate(source = ifelse(source == "CRAG | AS-EPA | NMSAS", "Ridge to Reef (CRAG|AS-EPA|NMSAS)", source),
         source = ifelse(source %in%  c("CRAG Longterm Manu'a Monitoring", "CRAG Longterm Tutuila Monitoring", "CRAG photolevel points"), "CRAG", source),
         source = ifelse(source == "ASNPS", "NPS AS", source),
         
         source_type = ifelse(source %in% c("Ridge to Reef (CRAG|AS-EPA|NMSAS)", "DMWR KRSP", "CRAG", "NPS AS", "AS-EPA", "NOAA - NCRMP"), "Long Term Monitoring", "Less Frequent Studies")) %>% 
  filter(data_type != "coral demography")

##remove demography data from inventory
    
  
## Save
## -----------------------------

write_csv(data_inventory, paste0(viz_path, "prep/data_inventory.csv"))
