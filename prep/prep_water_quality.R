#########################
#### Prep Water quality data
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
## AS-EPA 
## -----------------------------

asepa_raw <- read_csv(paste0(jv_path, "formatted_data/environmental/asepa_wq.csv"))

asepa <-asepa_raw %>% 
  mutate(end_date = NA,
         month = case_when(date == "June/July" ~ "06",
                           date == "July" ~ "07")) %>%
  mutate(start_date = as.character(paste0(year, "-", month, "-01"))) %>% 
 dplyr::select(-site, -month, -date, -geometry, -nearest_village) %>% 
  mutate(source = "AS-EPA",
         note = case_when(year == "2010" ~ "No survey day listed in report, first day of the month assigned to all surveys. In 2010 the month is listed as June/July in the report. For plotting purposes, we assigned June to all surveys conducted in that time period",
                          T ~ "No survey day listed in report, first day of the month assigned to all surveys. " ))

## -----------------------------
## R2R 
## -----------------------------

r2r_raw <- read_csv(paste0(jv_path, "formatted_data/environmental/R2R_wq.csv"))
r2r <- r2r_raw   %>% 
  separate(date, c("year", "month", "day"), sep = "-", remove = FALSE) %>% 
  dplyr::select(-site, -site_name_convention, -day, -month) %>% 
  mutate(end_date = NA) %>% 
  mutate(start_date = as.character(date),
         note = NA) %>% 
  dplyr::select(-date) %>% 
  filter(!is.na(latitude),
         !is.na(longitude)) %>% 
  mutate(source = "Ridge to Reef (CRAG|AS-EPA|NMSAS)")

## -----------------------------
## NOAA-Whitall-Fagatele 
## -----------------------------

whitall_fagatele_raw <- read_csv(paste0(jv_path, "formatted_data/environmental/noaa_whitall_fagatele.csv"))

ch <- whitall_fagatele_raw %>% 
  filter(!is.na(end_date))
unique(ch$start_date)
c1 <- unique(ch$parameter)
c1 <- paste(unique(ch$parameter), collapse = ", ")

# ch2 <- whitall_fagatele %>% 
#   filter(is.na(end_date))
# unique(ch2$start_date)
# c2 <-unique(ch2$parameter)

whitall_fagatele <- whitall_fagatele_raw %>% 
  mutate(start_date = as.character(start_date),
         note = paste0("Some surveys took place over the span of 3 days. These include the surveys that sampled:", c1)) %>% 
  dplyr::select(-para_unit) %>% 
  mutate(year = "2019")



## -----------------------------
## NOAA-Whitall-Vatia 
## -----------------------------

whitall_vatia_raw <- read_csv(paste0(jv_path, "formatted_data/environmental/noaa_whitall_vatia.csv"))

whitall_vatia <- whitall_vatia_raw  %>% 
  mutate(source = "NOAA - Whitall - Vatia") %>% 
  mutate(start_date = as.character(start_date),
         note = NA) %>% 
  dplyr::select(-para_unit)
## -----------------------------
## NOAA-Aua
## -----------------------------

smith_aua_raw <- read_csv(paste0(jv_path, "formatted_data/environmental/noaa_smith_aua.csv"))
smith_aua <- smith_aua_raw %>% 
  dplyr::select(-date_time) %>% 
  mutate(start_date = as.character(start_date),
         unit = ifelse(parameter == "pH", "total pH", unit),
         note = NA)


## -----------------------------
## Combine
## -----------------------------

wq_comb <- rbind(asepa, r2r, whitall_vatia, whitall_fagatele, smith_aua) %>% 
  filter(!is.na(value),
         !is.na(latitude)) %>% 
  filter(parameter != "temperature") %>% 
  mutate(lat = latitude,
         lon = longitude) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  mutate(parameter = case_when(parameter == "entero" ~ "enterococcus (fecal matter indicator",
                               T ~ parameter),
         parameter_unit = as.character(paste0(parameter, " (", unit, ")")),
         parameter = ifelse(parameter == "chl_a", "chl a", parameter)) %>% 
  mutate(common_parameter_binary = ifelse(parameter %in% c("TP", "TN", "DIP", "DIN",  "DO", "pH", "salinity", "nitrite", "nitrate" , "ammonia", "NO3-", "HPO4=", "NH4+", "NO2-", "Urea" , "chl a" , "nitrate and nitrite", "phosphate", "silicate","HSIO3-"), "common parameter", "less common parameter"))





  # mutate(value = case_when(unit == "TN_mg N/L" ~ value*1000,
  #                          T ~ value),
  #        unit = case_when(unit == "TN_mg N/L" ~ "TN_μg/L",
  #                         T ~ unit),
  #        value = case_when(unit == "TP_mg P/L" ~ value*1000,
  #                          T ~ value),
  #        unit = case_when(unit == "TP_mg P/L" ~ "TP_μg/L",
  #                         T ~ unit))


write_csv(wq_comb, paste0(viz_path, "prep/insitu_water_quality.csv"))


