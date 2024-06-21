##### Prep 
library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(mapview)
library(ncdf4)

jv_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_integration/"
viz_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_visulization/"

sat_env_para <- c("Bleaching_Alert_Area_7daymax_CRW_Daily", "Bleaching_Hotspot_CRW_Daily", "Chlorophyll_A_ESA_OC_CCI_v6.0_8Day", "Degree_Heating_Weeks_CRW_Daily", "Downward_Heat_Flux_GODAS_Monthly", "Isothermal_Layer_Depth_GODAS_Monthly", "Kd490_ESA_OC_CCI_8Day", "Marine_Heatwave_CRW_Daily",  "Mixed_Layer_Depth_GODAS_Monthly", "PAR_NASA_VIIRS_8Day", "Precipitation_CHIRPS_Daily", "Sea_Surface_Height_GODAS_Monthly")

#"Sea_Surface_Salinity_MirasSMOS_3Day", "Sea_Surface_Temperature_jplMUR_Daily", "Wave_Height_WW3_Global_Hourly", "Wave_Period_WW3_Global_Hourly", #"Wind_Speed_ASCAT_Daily", "Wind_Speed_Eastward_ASCAT_Daily", "Wind_Speed_Northward_ASCAT_Daily")

## load in most recent EDS run
load(paste0(jv_path, "EDS_downloads/EDS_Timeseries_2024-05-08.RData"))

combined <- data.frame(island = NA,
                        site_id = NA,
                        date = NA,
                        survey_method = NA,
                        nearest_village = NA,
                        lat = NA,
                        lon = NA,
                        eds_stat = NA,
                        parameter_long = NA,
                        parameter = NA,
                        value = NA,
                        unit = NA)

foreach(p = sat_env_para)%do%{
  
#  p <- sat_env_para[15]
  
  ## "Sea_Surface_Salinity_MirasSMOS_3Day" onward isn't downloaded, need to rerun EDS without it
  df_satellite <- df %>% 
    dplyr::select(-unit, -date_r, -DATA_UNIT) %>% 
    dplyr::select(1:7, contains(p)) %>% 
    dplyr::select(1:7, contains("DY01")) %>% 
    dplyr::select(1:7, contains("mean")) %>% 
    rename("value" = 8) %>% 
    mutate(parameter_long = p) %>% 
    mutate(parameter = case_when(parameter_long == "Bleaching_Alert_Area_7daymax_CRW_Daily" ~ "Bleaching Alert Area (CRW)",
                                 parameter_long == "Bleaching_Hotspot_CRW_Daily" ~ "Bleaching Hotspot (CRW)",
                                 parameter_long == "Chlorophyll_A_ESA_OC_CCI_v6.0_8Day" ~ "Chlorophyll a (ESA)",
                                 parameter_long == "Degree_Heating_Weeks_CRW_Daily" ~ "Degree Heating Weeks (CRW)",
                                 parameter_long == "Downward_Heat_Flux_GODAS_Monthly" ~ "Total Downward Heat Flux at Surface (GODAS)",
                                 parameter_long == "Isothermal_Layer_Depth_GODAS_Monthly" ~ "Bottom OCN Isothermal Layer Geometric Depth Below Sea Surface (GODAS)",
                                 parameter_long == "Kd490_ESA_OC_CCI_8Day" ~ "Kd490 (ESA)",
                                 parameter_long == "Marine_Heatwave_CRW_Daily" ~ "Marine Heatwave (CRW)",
                                 parameter_long == "Mixed_Layer_Depth_GODAS_Monthly" ~ "Bottom OCN Mixed Layer Geometric Depth Below Sea Surface  (GODAS)",
                                 parameter_long == "PAR_NASA_VIIRS_8Day" ~ "PAR (NASA VIIRS)",
                                 parameter_long ==  "Precipitation_CHIRPS_Daily" ~ "Precipitation (CHIRPS)" ,                 
                                 parameter_long ==  "Sea_Surface_Height_GODAS_Monthly" ~ "Sea Surface Height (GODAS)",          
                                 parameter_long ==  "Sea_Surface_Salinity_MirasSMOS_3Day" ~ "Sea Surface Salinity (MIRAS SMOS)",        
                                 parameter_long ==  "Sea_Surface_Temperature_jplMUR_Daily" ~ "Sea Surface Temperature (JPL MUR)",       
                                 parameter_long ==  "Wave_Height_WW3_Global_Hourly" ~ "Wave Height (WaveWatch III)" ,             
                                 parameter_long ==  "Wave_Period_WW3_Global_Hourly" ~ "Wave Period (WaveWatch III)",              
                                 parameter_long ==  "Wind_Speed_ASCAT_Daily" ~ "Wind Speed (ASCAT)",                     
                                 parameter_long ==  "Wind_Speed_Eastward_ASCAT_Daily" ~ "Wind Speed Eastward (ASCAT)" ,           
                                 parameter_long ==  "Wind_Speed_Northward_ASCAT_Daily" ~ "Wind Speed Northward (ASCAT)"),
           
           unit = case_when(parameter_long == "Bleaching_Alert_Area_7daymax_CRW_Daily" ~ "level",
                             parameter_long == "Bleaching_Hotspot_CRW_Daily" ~ "celsius",
                             parameter_long == "Chlorophyll_A_ESA_OC_CCI_v6.0_8Day" ~ "milligram m-3",
                             parameter_long == "Degree_Heating_Weeks_CRW_Daily" ~ "degree Celsius weeks",
                             parameter_long == "Downward_Heat_Flux_GODAS_Monthly" ~ "w/m^2",
                             parameter_long == "Isothermal_Layer_Depth_GODAS_Monthly" ~ "meters",
                             parameter_long == "Kd490_ESA_OC_CCI_8Day" ~ "m-1",
                             parameter_long == "Marine_Heatwave_CRW_Daily" ~ "category",
                             parameter_long == "Mixed_Layer_Depth_GODAS_Monthly" ~ "meters",
                             parameter_long == "PAR_NASA_VIIRS_8Day" ~ "einstein m^-2 day^-1",
                             parameter_long ==  "Precipitation_CHIRPS_Daily" ~ "mm/day)",                  
                             parameter_long ==  "Sea_Surface_Height_GODAS_Monthly" ~ "meters)",          
                             parameter_long ==  "Sea_Surface_Salinity_MirasSMOS_3Day" ~ "PSU)",        
                             parameter_long ==  "Sea_Surface_Temperature_jplMUR_Daily" ~ "degree C",       
                             parameter_long ==  "Wave_Height_WW3_Global_Hourly" ~ "meters" ,             
                             parameter_long ==  "Wave_Period_WW3_Global_Hourly" ~ "second",              
                             parameter_long ==  "Wind_Speed_ASCAT_Daily" ~ "m s-1",                     
                             parameter_long ==  "Wind_Speed_Eastward_ASCAT_Daily" ~ "m s-1",            
                             parameter_long ==  "Wind_Speed_Northward_ASCAT_Daily" ~ "m s-1"),
           eds_stat = "daily")
  
  combined <- rbind(combined, df_satellite)%>% 
    drop_na()

  
}


combined <- combined %>% 
  separate(date, c("year", "month", "day"), remove = FALSE) %>% 
  mutate(season = case_when(month %in% c("12", "01", "02") ~ "December - February",
                            month %in% c("03", "04", "05") ~ "March - May",
                            month %in% c("06", "07", "08") ~ "June - August",
                            month %in% c("09", "10", "11") ~ "September - November")) %>% 
  dplyr::select(-month, -day) %>%
  filter(island != "South_Bank",
         value != -9991) %>% 
  mutate(parameter_legend = case_when(parameter == "Bottom OCN Isothermal Layer Geometric Depth Below Sea Surface (GODAS)" ~ "Bottom OCN Isothermal Layer<br>Geometric Depth Below Sea<br>Surface (GODAS)",
                                      parameter == "Bottom OCN Mixed Layer Geometric Depth Below Sea Surface (GODAS)" ~ "Bottom OCN Mixed Layer<br>Geometric Depth Below Sea<br>Surface  (GODAS)",
                                      parameter == "Total Downward Heat Flux at Surface (GODAS)" ~ "Total Downward Heat Flux<br>at Surface (GODAS)",
                                      T ~ parameter),
         island = ifelse(island == "Ofu_&_Olosega", "Ofu & Olosega", island))

# data_inventory_t <- data_inventory %>% 
#   rename(lat = latitude, lon = longitude,
#          site_id = site_name)


saveRDS(combined,
     file = "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_visulization/prep/satellite_environmental_sites.rds")


## Prep average of each of the parameters by village - not sites. So this is just taking the satellite imagery
# 
# 
# satellite_path <- "S:/Data_Download"
# 
# islands <- c("Tau", "Tutuila", "Ofu_&_Olosenga", "Swains", "Rose")
# 
# sat_env_para <- c("Bleaching_Alert_Area_7daymax_CRW_Daily", "Bleaching_Hotspot_CRW_Daily", "Chlorophyll_A_ESA_OC_CCI_v6.0_8Day", "Degree_Heating_Weeks_CRW_Daily", "Downward_Heat_Flux_GODAS_Monthly", "Isothermal_Layer_Depth_GODAS_Monthly", "Kd490_ESA_OC_CCI_8Day", "Marine_Heatwave_CRW_Daily",  "Mixed_Layer_Depth_GODAS_Monthly", "PAR_NASA_VIIRS_8Day", "Precipitation_CHIRPS_Daily", "Sea_Surface_Height_GODAS_Monthly")
# 
# 
# foreach(para = sat_env_para)  %:% 
# 
#   para = sat_env_para[1]
# 
# foreach(i = islands)%do%{
#    
#   i <- islands[1]
#   
#   directory <- paste0(satellite_path, "/", para, "/Unit_Level_Data")
#   
#   file <- list.files(directory, pattern = i, full.names = TRUE)
#  
#   
#   nc_p = nc_open(file)
#   
#   # Pull var array
#   rawvar = ncvar_get(nc = nc_p, varid = as.vector(Parameter_Table$Fields[this_param_i]))
#   
#   # Pull dim vectors
#   lon = ncvar_get(nc = nc_p, varid = "longitude"); lon
#   lat = ncvar_get(nc = nc_p, varid = "latitude"); lat
#   rawt = ncvar_get(nc = nc_p, varid = "time"); rawt
#   
#   # Close nc
#   nc_close(nc_p)
#   
#   t = as_date(as_datetime(as.numeric(rawt), origin = ymd("1970/1/1")))
#   
# }
# 
# 
