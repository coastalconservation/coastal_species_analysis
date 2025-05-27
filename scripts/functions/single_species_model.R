# This function creates two rasters for species, a one for current habitat, one for habitat in 2050, 
# and one that shows the change in habitat

#' Title: single_species_model
#' Build and Project Ensemble Species Distribution Model (ESDM)
#'
#' Creates ensemble species distribution models (SDMs) for a specified species using historical and projected environmental rasters.
#' It outputs three rasters: current habitat suitability, projected habitat suitability in 2050, and the change in suitability.
#'
#' @param species Character string. The species name (must match `species_lump` in the biodiversity dataset).
#'
#' @return This function does not return a value. It saves three `.tif` raster files to disk:
#' - `current_<species>.tif`: Current suitability map.
#' - `projected_<species>.tif`: 2050 projected suitability.
#' - `ESDM_<species>_change.tif`: Change map between 2050 and present.
#'
#' @export
#'
#' @examples
#' single_species_model("Halymenia/Schizymenia spp")
#' 
single_species_model <- function(species){
  
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
  
  # Load Bio-Oracle environmental projection layers for 2050 (ssp460 scenario)
  # Each variable is loaded individually as a SpatRaster
  thetao_mean_50  <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/projected_ssp460/thetao_mean_50.tif")  # Ocean temperature
  tas_mean_50     <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/projected_ssp460/tas_mean_50.tif")     # Surface air temperature
  so_mean_50      <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/projected_ssp460/so_mean_50.tif")      # Salinity
  o2_mean_50      <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/projected_ssp460/o2_mean_50.tif")      # Dissolved oxygen
  mlotst_mean_50  <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/projected_ssp460/mlotst_mean_50.tif")  # Mixed layer depth
  clt_mean_50     <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/projected_ssp460/clt_mean_50.tif")     # Cloud cover
  
  # Combine future layers into a multi-layer SpatRaster object
  env_50_load <- c(thetao_mean_50,
                   tas_mean_50,
                   so_mean_50,
                   o2_mean_50,
                   mlotst_mean_50,
                   clt_mean_50)
  
  # Convert to a RasterStack for compatibility with SSDM
  env_50_load <- stack(env_50_load)
  
  # Load historical (baseline) Bio-Oracle layers using the same variables as above
  thetao_mean_00  <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/historical/thetao_mean_00.tif")
  tas_mean_00     <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/historical/tas_mean_00.tif")
  so_mean_00      <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/historical/so_mean_00.tif")
  o2_mean_00      <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/historical/o2_mean_00.tif")
  mlotst_mean_00  <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/historical/mlotst_mean_00.tif")
  clt_mean_00     <- rast("/capstone/coastalconservation/data/raw/BioOrc_rasters/historical/clt_mean_00.tif")
  
  # Combine historical rasters into a multi-layer SpatRaster
  env_00_load <- c(thetao_mean_00,
                   tas_mean_00,
                   so_mean_00,
                   o2_mean_00,
                   mlotst_mean_00,
                   clt_mean_00)
  
  # Convert to RasterStack format for modeling compatibility
  env_00_load <- stack(env_00_load)
  
  # Clean species name for use in filenames (replace slashes)
  species_clean <- gsub("/", "_", species)
  
  # Clean species names in the biodiversity dataset in the same way
  biodiv_2025 <- biodiv_2025 %>%
    mutate(species_lump_clean = gsub("/", "_", species_lump))
  
  # Filter biodiversity observations for the target species
  species_data <- biodiv_2025 %>% 
    filter(species_lump == species)
  
  # Group observations by location and sum total counts to get effort-adjusted presence data
  species_obs <- species_data %>%
    group_by(species_lump, latitude, longitude) %>%
    summarise(num_count = sum(total_count)) %>%
    ungroup()
  
  # Convert summed observations into presence/absence (occurrence = 1 if observed)
  species_pres_abs <- species_obs %>%
    mutate(occurrence = ifelse(num_count >= 1, 1, 0)) %>%
    dplyr::select(-num_count)
  
  # Subset again for the same species (can be redundant if already filtered above)
  species_subset <- subset(species_pres_abs, species_pres_abs$species_lump == species)
  
  # Convert to base data.frame for SSDM compatibility
  species_subset <- as.data.frame(species_subset)
  
  # Run the ensemble species distribution model (ESDM) using selected algorithms
  # Input data includes species presence/absence and historical environmental layers
  # `Pcol` specifies the binary occurrence column; `Xcol` and `Ycol` are spatial coordinates
  # Algorithms used: GBM, GAM, GLM, MARS, CTA, ANN, RF, SVM (MAXENT was excluded for this run)
  ESDM <- ensemble_modelling(
    c('GBM','GAM','GLM','MARS','CTA','ANN','RF','SVM'), 
    species_subset, 
    env_00_load, 
    Xcol = 'longitude', 
    Ycol = 'latitude', 
    Pcol = 'occurrence',
    verbose = FALSE
  )
  
  # Project the fitted ESDM onto future (2050) environmental conditions
  # This estimates future habitat suitability based on the same modeling framework
  ESDM_projection <- SSDM::project(ESDM, env_50_load)
  
  # Save the current suitability raster (based on historical conditions) to disk
  # Uses cleaned species name for a valid and consistent filename
  terra::writeRaster(
    ESDM@projection,
    filename = file.path(
      "/capstone/coastalconservation/data/processed/species_model_rasters/current_species_rasters",
      paste0("current_", gsub(" ", "_", species_clean), ".tif")
    ),
    overwrite = TRUE
  )
  
  # Save the projected suitability raster (based on 2050 conditions) to disk
  terra::writeRaster(
    ESDM_projection@projection,
    filename = file.path(
      "/capstone/coastalconservation/data/processed/species_model_rasters/projected_species_rasters",
      paste0("projected_", gsub(" ", "_", species_clean), ".tif")
    ),
    overwrite = TRUE
  )
  
  # Compute the difference in habitat suitability (2050 - current)
  # This represents predicted habitat change due to environmental shifts
  change_map <- ESDM_projection@projection - ESDM@projection
  
  # Save the habitat change raster to disk (for use in analysis or mapping)
  # File naming uses a consistent ESDM_ prefix to denote model output
  terra::writeRaster(
    change_map,
    filename = file.path(
      "/capstone/coastalconservation/data/processed/species_model_rasters/change_species_rasters",
      paste0("ESDM_", gsub(" ", "_", species_clean), "_change.tif")
    ),
    overwrite = TRUE
  )
  
  # Plot projected habitat suitability (2050 scenario)
  # Suitability values are visualized on a standardized scale of -1 to 1
  plot(
    ESDM_projection@projection,
    main = paste0("Habitat suitability\nfor ", species_clean),
    zlim = c(-1, 1)
  )
  
  # Plot habitat suitability change (2050 - present)
  # Visualizes where suitability is predicted to increase, decrease, or remain stable
  plot(
    change_map,
    main = paste0("Change in suitability (2050 - now)\nfor ", species_clean),
    zlim = c(-1, 1)
  )
}

