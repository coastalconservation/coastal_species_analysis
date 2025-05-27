auto_species_model <- function(biodiv_data) {
  
  species_list <- unique(biodiv_data$species_lump)
  
  for (species in species_list) {
    message("Running model for: ", species)
    tryCatch({
      single_species_model(species)
    }, error = function(e) {
      message("Error for species: ", species)
      message(e$message)
    })
  }
}
