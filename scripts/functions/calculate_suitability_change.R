#' calculate_suitability_change
#'
#' @description 
#' Calculates both raw and percent change in habitat suitability between current and projected rasters for a list of species. 
#' The function summarizes change for two spatial extents: full California statewide coverage and the Dangermond Preserve extent.
#'
#' @param species_list A character vector of species names (matching raster file naming conventions, with spaces automatically converted to underscores).
#'
#' @details 
#' For each species, the function reads current and projected habitat suitability rasters, computes total suitability values 
#' for California-wide and Dangermond Preserve extents, and calculates raw and percent changes in habitat suitability.
#' If no data exists for current suitability (sum = 0), percent change is handled with division-by-zero protection.
#'
#' @return A tibble containing the following columns for each species:
#' \describe{
#'   \item{species_name}{The species name used as input.}
#'   \item{raw_change_ca}{Raw (absolute) change in total habitat suitability statewide.}
#'   \item{percent_change_ca}{Percent change in habitat suitability statewide.}
#'   \item{raw_change_dangermond}{Raw (absolute) change in suitability within the Dangermond bounding box.}
#'   \item{percent_change_dangermond}{Percent change in suitability within the Dangermond bounding box.}
#' }
#'
#' @export
#'
#' @examples
#' species_list <- c("Alaria marginata", "Pisaster ochraceus")
#' calculate_suitability_change(species_list)
#' 
calculate_suitability_change <- function(species_list) {
  
  library(raster)
  library(dplyr)
  library(tibble)
  
  # Define raster directories
  current_dir <- "/capstone/coastalconservation/data/processed/species_model_rasters/current_species_rasters"
  projected_dir <- "/capstone/coastalconservation/data/processed/species_model_rasters/projected_species_rasters"
  
  # Define bounding box extent for Dangermond (using raster::extent)
  dangermond_extent <- extent(-121.597681, -118.117269, 34.35797, 35.24002)
  
  # Initialize results tibble
  results <- tibble(
    species_name = character(),
    raw_change_ca = numeric(),
    percent_change_ca = numeric(),
    raw_change_dangermond = numeric(),
    percent_change_dangermond = numeric()
  )
  
  # Loop through each species
  for (species in species_list) {
    species_safe <- gsub(" ", "_", species)
    
    # File paths
    current_file <- file.path(current_dir, paste0("current_", species_safe, ".tif"))
    projected_file <- file.path(projected_dir, paste0("projected_", species_safe, ".tif"))
    
    # Check if files exist
    if (!file.exists(current_file) | !file.exists(projected_file)) {
      warning(paste("Files not found for species:", species))
      next
    }
    
    # Load rasters using raster package
    current_r <- raster(current_file)
    projected_r <- raster(projected_file)
    
    # CA-wide (unclipped) - using cellStats
    current_sum_ca <- cellStats(current_r, stat = "sum", na.rm = TRUE)
    projected_sum_ca <- cellStats(projected_r, stat = "sum", na.rm = TRUE)
    raw_change_ca <- projected_sum_ca - current_sum_ca
    
    # Handle division by zero
    if (current_sum_ca == 0) {
      percent_change_ca <- ifelse(projected_sum_ca > 0, Inf, 0)
    } else {
      percent_change_ca <- ((projected_sum_ca - current_sum_ca) / current_sum_ca) * 100
    }
    
    # Dangermond cropped
    current_crop <- crop(current_r, dangermond_extent)
    projected_crop <- crop(projected_r, dangermond_extent)
    current_sum_dangermond <- cellStats(current_crop, stat = "sum", na.rm = TRUE)
    projected_sum_dangermond <- cellStats(projected_crop, stat = "sum", na.rm = TRUE)
    raw_change_dangermond <- projected_sum_dangermond - current_sum_dangermond
    
    # Handle division by zero for Dangermond
    if (current_sum_dangermond == 0) {
      percent_change_dangermond <- ifelse(projected_sum_dangermond > 0, Inf, 0)
    } else {
      percent_change_dangermond <- ((projected_sum_dangermond - current_sum_dangermond) / current_sum_dangermond) * 100
    }
    
    # Append to results
    results <- results %>%
      add_row(
        species_name = species,
        raw_change_ca = raw_change_ca,
        percent_change_ca = percent_change_ca,
        raw_change_dangermond = raw_change_dangermond,
        percent_change_dangermond = percent_change_dangermond
      )
  }
  
  return(results)
}