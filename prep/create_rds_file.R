### Create RDS file

viz_path <- "//PICKINGFISH/USERS/juliette.verstaen/american_samoa_visulization/"

## make sure to rerun the prep script if needed 

data_inventory                       <- read_csv(paste0(viz_path, "prep/data_inventory.csv"))
datasource_timeline                  <- read_csv(paste0(viz_path, "prep/datasource_timeline.csv"))

benthic_cover_group                  <- read_csv(paste0(viz_path, "prep/benthic_cover_group.csv"))
benthic_cover_village                <- read_csv(paste0(viz_path, "prep/benthic_cover_village.csv"))
benthic_island_summarized            <- read_csv(paste0(viz_path, "prep/benthic_island_summarized.csv")) 
benthic_island_summarized_labs       <- read_csv(paste0(viz_path, "prep/benthic_island_summarized_labs.csv")) 

fish_village                           <- read_csv(paste0(viz_path, "prep/fish_village_biomass_density.csv"))
fish_island                            <- read_csv(paste0(viz_path, "prep/fish_island_biomass_density.csv"))

# fish_count                           <- read_csv(paste0(viz_path, "prep/fish_count.csv"))
# fish_island_summarized               <- read_csv(paste0(viz_path, "prep/fish_island_functional.csv"))
# fish_island_summarized_labs          <- read_csv(paste0(viz_path, "prep/fish_island_summarized_labs.csv"))

insitu_temperature                   <- read_csv(paste0(viz_path, "prep/insitu_temperature.csv"))
water_quality                        <- read_csv(paste0(viz_path, "prep/insitu_water_quality.csv"))

# satellite_environmental_sst          <- readRDS(paste0(viz_path, "prep/satellite_environmental_sst.rds"))
# satellite_environmental_chl_a        <- readRDS(paste0(viz_path, "prep/satellite_environmental_chl_a.rds"))

satellite_environmental              <- readRDS(paste0(viz_path, "prep/satellite_environmental_sites.rds"))

events_timeline                      <- read_csv(paste0(viz_path, "prep/events_timeline.csv"))

## save as .RDS file

save(data_inventory,
     datasource_timeline,
     benthic_cover_group,
     fish_village,
     fish_island,
     benthic_cover_village,
     benthic_island_summarized,
     benthic_island_summarized_labs,
     # fish_count,
     # fish_island_summarized,
     # fish_island_summarized_labs,
     satellite_environmental,
     insitu_temperature,
     water_quality,
     events_timeline,
     file = "dashboard/data/data.RData")
