tmap_habitat_change <- function(species, save_plot = TRUE) {
  # Define file path for the change raster
  change_path <- here::here("scripts", "modeling", "future_change_output_rasters", 
                            paste0("ESDM_", gsub(" ", "_", species), "_change.tif"))
  #/capstone/coastalconservation/data/processed/species_model_rasters/change_species_rasters
  # Load the change raster
  change_rast <- rast(change_path)
  
  # Create tmap
  tmap_mode("view")
  
  map <- tm_shape(change_rast) +
    tm_raster(midpoint = 0,
              breaks = seq(-1, 1, by = 0.2),
              palette = "PuOr",
              title = "Change in\nHabitat Suitability") +
    # either esri ocean or a more plane one 
    tm_basemap("Esri.OceanBasemap") +
    tm_layout(
      main.title = paste0("Change in Suitability (2050)\n", species),
      legend.outside = TRUE
    )
  
  print(map)
}



