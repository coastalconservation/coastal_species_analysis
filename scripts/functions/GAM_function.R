GAM_function <- function(species){
  
  # Load packages
  library(tidyverse)
  library(dplyr)
  library(SSDM)
  library(biooracler)
  library(raster)
  library(terra)
  library(purrr)
  library(shinyFiles) # For ssdm shiny interface
  library(here)
  library(sf)
  
  # Load functions 
  source(here::here('scripts', 'functions', 'clean_biodiv.R'))
  
  # Set seed for reproducable effects
  set.seed(27)
  
  # Load biodiversity data
  biodiv_2025 <- clean_isles_biodiv(cbs_excel_name = 'cbs_data_2025.xlsx', 
                                    point_contact_sheet = 'point_contact_summary_layered',
                                    quadrat_sheet = 'quadrat_summary_data',
                                    swath_sheet = 'swath_summary_data')
  
  # Load Bio-Oracle data and turn into rasterstack
  
  # Manually read in the rasters
  thetao_mean_50  <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/projected_ssp460/thetao_mean_50.tif")
  tas_mean_50     <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/projected_ssp460/tas_mean_50.tif")
  so_mean_50      <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/projected_ssp460/so_mean_50.tif")
  o2_mean_50      <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/projected_ssp460/o2_mean_50.tif")
  mlotst_mean_50  <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/projected_ssp460/mlotst_mean_50.tif")
  clt_mean_50     <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/projected_ssp460/clt_mean_50.tif")
  
  # Combine into one multi-layer object
  env_50_load <- c(thetao_mean_50,
                   tas_mean_50,
                   so_mean_50,
                   o2_mean_50,
                   mlotst_mean_50,
                   clt_mean_50)
  # Stack raster
  env_50_load <- stack(env_50_load)
  
  # Manually read in the historical rasters
  thetao_mean_00  <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/historical/thetao_mean_00.tif")
  tas_mean_00     <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/historical/tas_mean_00.tif")
  so_mean_00      <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/historical/so_mean_00.tif")
  o2_mean_00      <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/historical/o2_mean_00.tif")
  mlotst_mean_00  <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/historical/mlotst_mean_00.tif")
  clt_mean_00     <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/historical/clt_mean_00.tif")
  
  # Combine into a single multi‐layer SpatRaster
  env_00_load <- c(
    thetao_mean_00,
    tas_mean_00,
    so_mean_00,
    o2_mean_00,
    mlotst_mean_00,
    clt_mean_00
  )
  # Stack raster
  env_00_load <- stack(env_00_load)
  
  species_clean <- gsub("/", "_", species)
    
  biodiv_2025 <- biodiv_2025 %>%
    mutate(species_lump_clean = gsub("/", "_", species_lump))
  
  species_data <- biodiv_2025 %>% 
    filter(species_lump == species) 
  
  species_obs <- species_data |>
    group_by(species_lump, latitude, longitude) |>
    summarise(num_count = sum(total_count)) |>
    ungroup()
  
  # Expand rows & keep observation column
  species_pres_abs <- species_obs %>%
    mutate(occurrence = ifelse(num_count >= 1, 1, 0)) %>% 
    dplyr::select(-num_count)
  
  # Subset the species
  species_subset <- subset(species_pres_abs, species_pres_abs$species_lump == species)
  
  # Turn subset into data frame
  species_subset <- as.data.frame(species_subset)
    
    
    SDM_glm <- modelling('GAM', species_subset, 
                         env_00_load, Xcol = 'longitude', Ycol = 'latitude', verbose = FALSE)
    plot(SDM_glm@projection, main = "SDM\nfor", species, "\nwith GLM algorithm")
    
    ESDM_projection <- SSDM::project(ESDM, env_50_load)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  # Corrected version using file.path
  terra::writeRaster(ESDM@projection,
                     filename = file.path("/capstone/coastalconservation/data/processed/species_model_rasters/current_species_rasters",
                                          paste0("current_", gsub(" ", "_", species), ".tif")),
                     overwrite = TRUE
  )
  
  
  # Write raster into file
  terra::writeRaster(ESDM_projection@projection, filename = file.path(
    "/capstone/coastalconservation/data/processed/species_model_rasters/projected_species_rasters",
    paste0("projected_", gsub(" ", "_", species), ".tif")),
    overwrite = TRUE
  )
  
  # Calculate change in suitable habitat
  change_map <- ESDM_projection@projection - ESDM@projection
  
  # Save change map to new folder
  terra::writeRaster(change_map, filename = file.path("/capstone/coastalconservation/data/processed/species_model_rasters/change_species_rasters",
                                                      paste0("ESDM_", gsub(" ", "_", species), "_change.tif")), 
                     overwrite = TRUE)
  
  # Show plot
  plot(ESDM_projection@projection, main = paste0("Habitat suitability\nfor ", species), zlim = c(-1, 1))
  
  # Show change in suitable habitat plot
  plot(change_map, main = paste0("Change in suitability (2050 - now)\nfor ", species),
       zlim = c(-1, 1))
}