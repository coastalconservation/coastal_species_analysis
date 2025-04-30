model_sing_species <- function(species){
  
  # Load in Libraries
  
  library(tidyverse)
  library(dplyr)
  library(SSDM)
  library(biooracler)
  library(raster)
  library(terra)
  library(here)
  
  # Source external R scripts for custom functions
  source(here::here('scripts', 'functions', 'clean_biodiv.R'))
  
  # Load biodiversity data
  biodiv_2025 <- clean_biodiv(cbs_excel_name = 'cbs_data_2025.xlsx', 
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
  
  # Clean Species data
  species_data <- biodiv_2025 %>% 
    filter(species_lump == species)
  
  species_obs <- species_data |>
    group_by(species_lump, latitude, longitude) |>
    summarise(num_count = sum(total_count)) |>
    ungroup()
  
  species_obs_present <- species_obs |>
    filter(num_count >=1 ) |>
    uncount(num_count)
  
  # Model species with SDM
  ESDM <- ensemble_modelling(c('GAM', 'GLM','MARS', 'CTA','GBM', 'MAXENT', 'ANN', 'RF', 'SVM'), subset(species_obs_present, species_obs_present$species_lump == species),
                             env_00_load, rep = 1, Xcol = 'longitude', Ycol = 'latitude',
                             ensemble.thresh = 0, verbose = FALSE)
  plot(ESDM@projection, main = 'ESDM\nfor Species\nwith GAM and GLM algorithms')
 
   # Plot just the model
  plot(ESDM)
}

