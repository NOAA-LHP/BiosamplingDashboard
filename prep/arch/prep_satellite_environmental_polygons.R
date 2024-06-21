##### Prep 
library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(mapview)

jv_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/"
viz_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_visulization/"


## Coral Reef Watch SST
load(paste0(viz_path, "polygon_satellite_summaries/SST_CRW_2005_2020.RData"))

# sst_sat_crw <- df %>% 
#   dplyr::select(island = DATA_UNIT,
#                 site_long = ClustID,
#                 StartDate,
#                 longitude = CenLon,
#                 latitude = CenLat,
#                 mean_SST = mean_Sea_Surface_Temperature_CRW_Daily_SPAN,
#                 q05_SST = q05_Sea_Surface_Temperature_CRW_Daily_SPAN,
#                 q95_STT = q95_Sea_Surface_Temperature_CRW_Daily_SPAN,
#                 sd_STT = sd_Sea_Surface_Temperature_CRW_Daily_SPAN) %>% 
#   separate(site_long, c("remove1", "village", "year", "remove3"), sep = "_") %>% 
#   dplyr::select(-remove1, -remove3) %>% 
#   mutate(date = as.Date(StartDate)) %>% 
#   dplyr::select(-StartDate) %>% 
#  #pivot_longer(cols = c("mean", "q05", "q95", "sd"), names_to = "stat") %>% 
#  #mutate(value = ifelse(stat == "mean", round(value, digits = 2), value)) %>% 
#   #mutate(parameter = "SST") %>% 
#   mutate(year_month = str_sub(date, end = -4)) %>% 
#   st_drop_geometry()

## JPL SST

load(paste0(viz_path, "polygon_satellite_summaries/SST_JPL_2005_2020.RData"))

sst_sat_jpl <- df %>% 
  dplyr::select(island = DATA_UNIT,
                site_long = ClustID,
                StartDate,
                longitude = CenLon,
                latitude = CenLat,
                mean_SST = mean_Sea_Surface_Temperature_jplMUR_Daily_SPAN,
                q05_SST = q05_Sea_Surface_Temperature_jplMUR_Daily_SPAN,
                q95_STT = q95_Sea_Surface_Temperature_jplMUR_Daily_SPAN,
                sd_STT = sd_Sea_Surface_Temperature_jplMUR_Daily_SPAN) %>% 
  mutate(site_long = str_replace(site_long, "Ofu_&_Olosega", "Ofu-&-Olosega")) %>% 
  separate(site_long, c("remove1", "village", "year", "remove3"), sep = "_") %>% 
  dplyr::select(-remove1, -remove3) %>% 
  mutate(date = as.Date(StartDate)) %>% 
  dplyr::select(-StartDate) %>% 
  #pivot_longer(cols = c("mean", "q05", "q95", "sd"), names_to = "stat") %>% 
  #mutate(value = ifelse(stat == "mean", round(value, digits = 2), value)) %>% 
  #mutate(parameter = "SST") %>% 
  mutate(year_month = str_sub(date, end = -4),
         island_village = paste0(island, ":", village))
#%>% st_drop_geometry()


## Chlorophyll

load(paste0(viz_path, "polygon_satellite_summaries/Chla_CRW_2005_2020.RData"))

chl_sat_crw <- df %>% 
  dplyr::select(island = DATA_UNIT,
                site_long = ClustID,
                StartDate,
                longitude = CenLon,
                latitude = CenLat,
                mean_chl_a = mean_Chlorophyll_A_ESA_OC_CCI_v6.0_8Day_SPAN,
                q05_chl_a = q05_Chlorophyll_A_ESA_OC_CCI_v6.0_8Day_SPAN,
                q95_chl_a = q95_Chlorophyll_A_ESA_OC_CCI_v6.0_8Day_SPAN,
                sd_chl_a = sd_Chlorophyll_A_ESA_OC_CCI_v6.0_8Day_SPAN) %>% 
  mutate(site_long = str_replace(site_long, "Ofu_&_Olosega", "Ofu-&-Olosega")) %>% 
  separate(site_long, c("remove1", "village", "year", "remove3"), sep = "_") %>% 
  dplyr::select(-remove1, -remove3) %>% 
  mutate(date = as.Date(StartDate)) %>% 
  dplyr::select(-StartDate) %>% 
  #pivot_longer(cols = c("mean", "q05", "q95", "sd"), names_to = "stat") %>% 
  #mutate(value = ifelse(stat == "mean", round(value, digits = 2), value)) %>% 
  #mutate(parameter = "SST") %>% 
  mutate(year_month = str_sub(date, end = -4),
         island_village = paste0(island, ":", village))
#%>% st_drop_geometry()



#satellite_environmental <- left_join(sst_sat_jpl) 
## add back on the st_as_sf

satellite_environmental_sst <- sst_sat_jpl
satellite_environmental_chl_a <- chl_sat_crw

saveRDS(satellite_environmental_sst,
     file = "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_visulization/prep/satellite_environmental_sst.rds")
saveRDS(satellite_environmental_chl_a,
     file = "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_visulization/prep/satellite_environmental_chl_a.rds")


