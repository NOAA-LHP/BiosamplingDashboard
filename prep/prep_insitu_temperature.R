##############################
## Prep insitu temperature data
##############################

## -----------------------------
## load libraries and paths
## -----------------------------
library(tidyverse)
library(mapview)
library(sf)

jv_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/"
viz_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_visulization/"

## -----------------------------
## load in the sector shapefiles to assign sites to sector
## -----------------------------
tut_sector <- read_sf(dsn = "//PICBILLFISH/GENERAL/Benthic/Common/Maps/AMSM/TUT/Sector", layer = "TUT_sector") %>%
  filter(SEC_NAME != "TUT_LAND") %>% 
  mutate(island = "Tutuila")

tau_sector <- read_sf(dsn = "//PICBILLFISH/GENERAL/Benthic/Common/Maps/AMSM/TAU/Sector", layer = "TAU_sector") %>%
  filter(SEC_NAME != "TAU_LAND")%>% 
  mutate(island = "Tau")

swa_sector <- read_sf(dsn = "//PICBILLFISH/GENERAL/Benthic/Common/Maps/AMSM/SWA/Sector", layer = "SWA_sector") %>%
  filter(SEC_NAME != "SWA_LAND") %>% 
  mutate(island = "Swains")

ros_sector <- read_sf(dsn = "//PICBILLFISH/GENERAL/Benthic/Common/Maps/AMSM/ROS/Sector", layer = "ROS_sector") %>%
  filter(SEC_NAME != "ROS_LAND") %>% 
  dplyr::select(-Id)%>% 
  mutate(island = "Rose")

ofu_sector <- read_sf(dsn = "//PICBILLFISH/GENERAL/Oceanography/Carbonate Chemistry Analysis/Sectors/", layer = "island_sectors_merged")  %>%
  filter(sector_nam == "OFU_OLOSEGA") %>%
  mutate(sector = "Ofu & Olosega",
         island = "Ofu & Olosega",
         sector_code = sector_nam) %>% 
  dplyr::select(sector, island, sector_code)

amsm_sectors <- rbind(tut_sector,tau_sector, swa_sector, ros_sector) %>% 
  dplyr::select(island, sector = Sector, sector_code = SEC_NAME) %>% 
    sf::st_transform(4326) %>% 
  rbind(ofu_sector) %>% 
  st_make_valid()
  
## there are some weird vertex intersections happen - so fix it
#sf_use_s2(amsm_sectors)
all(st_is_valid(amsm_sectors))
library(spdep)
nb <- poly2nb(amsm_sectors)


## -----------------------------
## Clean up the CRAG temperature loggers
## -----------------------------

load("//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/formatted_data/environmental/crag_temp_loggers.RData")

crag_loggers <- crag_loggers_coords %>% 
  mutate(site_id = paste0(site, "-logger", logger_number),
         reef_type = ifelse(reef_type == "pool", "lagoon", reef_type)) %>% 
  mutate(date_time = as.POSIXlt(date_time),
         lat = latitude,
         lon = longitude) %>%
  filter(!is.na(lat),
         !is.na(lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(island = "Tutuila",
         depth = NA) %>% 
  mutate(depth = case_when(is.na(depth) & reef_type == "lagoon" ~ 3,
                           is.na(depth) & reef_type == "slope" ~ 10,
                           is.na(depth) & reef_type == "reef flat" ~ 3,
                           T ~ depth),
                           source = "CRAG",
         year = as.numeric(year)) %>% 
  dplyr::select(site_id,island, reef_type, year, date_time, source, latitude, depth, longitude, temp_C)


## -----------------------------
## Clean up the NOAA StRs
## -----------------------------

load("//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/formatted_data/environmental/NCRMP_StRs.RData")

noaa_strs <- str_2023 %>% 
  filter(!is.na(date_time)) %>% 
  separate(date_time, c("duplicate", "time"), sep = " ") %>% 
  
  separate(time, c("hour", "minute"), sep = ":") %>% 
  mutate(hour = str_pad(hour, width = 2, side = "left", pad = "0"),
         second = "00") %>% 
  unite(time, c("hour", "minute", "second"), sep = ":") %>% 
  
  separate(date, c("month", "day", "year2"), sep = "/") %>% 
  mutate(day = str_pad(day, width = 2, side = "left", pad = "0"),
         month = str_pad(month, width = 2, side = "left", pad = "0")) %>% 
  unite(date, c("year2", "month", "day"), sep = "-") %>% 
  
  unite(date_time, c("date", "time"), sep = " ") %>% 
  mutate(date_time = as.POSIXlt(date_time),
         lat = latitude, 
         lon = longitude) %>%
  filter(!is.na(lat),
         !is.na(lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  mutate(source = "NOAA - NCRMP") %>% 
  dplyr::select(site_id, island, reef_type, year, date_time, source, latitude, longitude, depth, temp_C)


## -----------------------------
## Aua LBSP project temperature measurments
## -----------------------------

smith_aua_raw <- read_csv(paste0(jv_path, "formatted_data/environmental/noaa_smith_aua_temperature.csv"))
smith_aua <- smith_aua_raw %>% 
  mutate(date_time = as.POSIXlt(date_time),
         lat = latitude, 
         lon = longitude) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  mutate(island = "Tutuila",
         source = "NOAA - Smith") %>% 
  dplyr::select(site_id, island, reef_type, year, date_time, source, latitude, longitude, depth, temp_C = temperature_C)


## -----------------------------
## Combine CRAG and NOAA data, add in the sector info, and save
## -----------------------------

insitu_temp_data <- rbind(smith_aua, noaa_strs, crag_loggers) %>% 
  filter(!is.na(year)) %>% 
  mutate(temp_C = as.numeric(temp_C)) %>% 
  filter(!is.na(temp_C)) %>% 
  separate(date_time, c("date", "time"), sep = " ") %>% 
  group_by(site_id, island, reef_type, year, date, latitude, longitude, depth, source) %>% 
  summarize(daily_mean_temp_C = mean(temp_C)) %>% 
  ungroup() %>% 
  sf::st_join(amsm_sectors) %>% 
  rename(island = island.x) %>% 
  mutate(island_sector = paste0(island, "-", sector))  %>% 
  dplyr::select(-island.y) %>% 
  mutate(depth_strata_short = case_when(depth < 10 ~ "shallow",
                                        depth >=10 & depth < 20 ~ "middle",
                                        depth >=20 ~ "deep"),
         depth_strata_long = case_when(depth < 10 ~ "shallow (0-10 m)",
                                       depth >=10 & depth < 20 ~ "middle (10-20 m)",
                                       depth >=20 ~ "deep (>20 m)")) %>% 
  mutate(depth_strata_short = ifelse(site_id == "Alofau-logger2","shallow",depth_strata_short),
         depth_strata_short = ifelse(site_id == "Alofau-logger1","shallow",depth_strata_short),
         depth_strata_long = ifelse(site_id == "Alofau-logger2", "shallow (0-10 m)", depth_strata_long),
         depth_strata_long = ifelse(site_id == "Alofau-logger1", "shallow (0-10 m)", depth_strata_long)) %>% 
  mutate(daily_mean_temp_C = round(daily_mean_temp_C, digits = 2)) 

#mapview(insitu_temp_data)


## add in villages


tut_shape <- read_sf(paste0(jv_path, "spatial/tutuila_villages/tut_villages.shp")) %>% 
  st_transform(crs = 4326)

insitu_temp_data_sf <- insitu_temp_data %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

sites_df <- insitu_temp_data_sf  %>%  
  dplyr::select(island, site_id, source) %>% 
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

insitu_temp_data <- insitu_temp_data_sf %>% 
  st_join(villages) 

### ---- look before saving

write_csv(insitu_temp_data, paste0(viz_path, "prep/insitu_temperature.csv"))

# unique(insitu_temp_data$source)
# temp <- filter(insitu_temp_data, source == "NOAA - Smith - Aua")
