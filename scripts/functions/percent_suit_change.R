percent_suit_change <- function(species_list){
  
  # Load libraries
  library(raster)
  library(tibble)
  library(dplyr)
  
  # Define raster directories
  current_dir <- "/capstone/coastalconservation/data/processed/species_model_rasters/current_species_rasters"
  future_dir  <- "/capstone/coastalconservation/data/processed/species_model_rasters/projected_species_rasters"
  
  # Define bounding box extent for Point Conception
  bbox_extent <- extent(-121.597681, -118.117269, 34.35797, 35.24002)
  
  # Initialize results tibble
  results <- tibble(species_lump = character(), percent_change = numeric())
  
  # Loop through species
  for (species in species_list) {
    species_safe <- gsub(" ", "_", species)
    
    # File paths
    current_file <- file.path(current_dir, paste0("current_", species_safe, ".tif"))
    future_file  <- file.path(future_dir,  paste0("projected_", species_safe, ".tif"))
    
    # Load and crop rasters
    current <- raster(current_file)
    future  <- raster(future_file)
    
    current_crop <- crop(current, bbox_extent)
    future_crop  <- crop(future, bbox_extent)
    
    # Calculate sums and percent change
    current_sum <- cellStats(current_crop, 'sum', na.rm = TRUE)
    future_sum  <- cellStats(future_crop, 'sum', na.rm = TRUE)
    
    # Protect against division by zero
    if (current_sum == 0) {
      percent_change <- NA
    } else {
      percent_change <- ((future_sum - current_sum) / current_sum) * 100
    }
    
    # Append to results
    results <- results %>%
      add_row(species_lump = species, percent_change = percent_change)
  }
  
  return(results)
  
}
