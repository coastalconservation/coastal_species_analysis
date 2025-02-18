#' Clean MARINe Coastal Biodiversity Surveys Data 
#'
#' @description
#' This is a function that takes the excel file given when you request the MARINe Coastal Biodiversity Survey (CBS) and combines the valued sheets into a single dataframe that can be used to into a single dataframe to be used for present absent analysis. 
#' 
#' @param cbs_excel_name The name of the MARINe CBS excel file. Written in quotes " " 
#' @param point_contact_layer The exact name of the point contact summary sheet. Written in quotes " "
#' @param quadrat_layer The exact name of the quadrat summary sheet. Written in quotes " "
#' @param swath_layer The exact name of the swath summary sheet. Written in quotes " " 
#'
#' @returns Combines the 3 excel sheets into a single dataframe
#' @export
#'
#' @examples
MarineBioClean <- function(cbs_excel_name, point_contact_sheet, quadrat_sheet, swath_sheet) {
  #..........................Create paths..........................
  input_path <- here::here('data', 'raw', cbs_excel_name)
  output_folder <- here::here('data', 'processed')
  output_file <- file.path(output_folder, "clean_biodiv.csv")
  
  #.........................Read in sheets.........................
  point_contact_raw <- readxl::read_excel(input_path, sheet = point_contact_sheet)
  quadrat_raw <- readxl::read_excel(input_path, sheet = quadrat_sheet)
  swath_raw <- readxl::read_excel(input_path, sheet = swath_sheet) 
  
  #..................Clean sheets to prep for merge.................
  # Clean point_contact dataset 
  point_contact_clean <- point_contact_raw %>% 
    # Remove non-matching columns 
    select(!c('number_of_transect_locations', 'percent_cover')) %>% 
    # Rename num of hits to total count 
    rename(total_count = number_of_hits) %>% 
    # Create new data collection source column 
    mutate(collection_source = "point contact") %>% 
    # Filter to mainland only 
    filter(island == "Mainland") %>% 
    # Remove certain species lumps 
    filter(!species_lump %in% c("Rock", "Sand", "Tar", "Blue Green Algae", "Red Crust", "Diatom", "Ceramiales"))
  
  # Clean quadrat dataset 
  quadrat_clean <- quadrat_raw %>% 
    # Remove non-matching columns 
    select(!c('number_of_quadrats_sampled', 'total_area_sampled_m2', 'density_per_m2')) %>% 
    # Create new data collection source column 
    mutate(collection_source = "quadrat") %>% 
    # Filter to mainland only 
    filter(island == "Mainland") %>% 
    # Remove certain species lumps 
    filter(!species_lump %in% c("Rock", "Sand", "Tar", "Blue Green Algae", "Red Crust", "Diatom", "Ceramiales"))
  
  # Clean swath dataset 
  swath_clean <- swath_raw %>% 
    # Remove non-matching columns 
    select(!c('number_of_transects_sampled', 'est_swath_area_searched_m2',  'density_per_m2')) %>% 
    # Create new data collection source column 
    mutate(collection_source = "swath") %>% 
    # Filter to mainland only 
    filter(island == "Mainland") %>% 
    # Remove certain species lumps 
    filter(!species_lump %in% c("Rock", "Sand", "Tar", "Blue Green Algae", "Red Crust", "Diatom", "Ceramiales"))
  
  #....................Merge datasets together.....................
  clean_biodiv <- bind_rows(point_contact_clean, quadrat_clean, swath_clean)
  
  #........................Save as .csv file.......................
  write.csv(clean_biodiv, output_file, row.names = FALSE)
  
  # Return merged dataframe 
  return(clean_biodiv)
}