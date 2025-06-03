#' Clean MARINe Coastal Biodiversity Surveys Data 
#'
#' @description
#' #' This function reads and merges MARINe Coastal Biodiversity Survey (CBS) Excel sheets into a combined 
#' dataset for presence-absence analysis. The cleaned output includes data from both mainland and island sites.
#' 
#' @param cbs_excel_name The filename of the MARINe CBS Excel file (e.g., "cbs_data_2025.xlsx")
#' @param point_contact_sheet Exact name of the point contact summary sheet
#' @param quadrat_sheet Exact name of the quadrat summary sheet
#' @param swath_sheet Exact name of the swath summary sheet
#'
#' @return A cleaned, combined dataframe
#' @export
#'
#' @examples
clean_isles_biodiv <- function(cbs_excel_name = 'cbs_data_2025.xlsx',
                         point_contact_sheet = 'point_contact_summary_layered',
                         quadrat_sheet = 'quadrat_summary_data',
                         swath_sheet = 'swath_summary_data') {
  #.........................Create full path.........................
  input_path <- file.path("/capstone/coastalconservation/data/raw/MARINe_data/biodiversity", cbs_excel_name)
  
  #.........................Read in sheets..........................
  point_contact_raw <- readxl::read_excel(input_path, sheet = point_contact_sheet)
  quadrat_raw <- readxl::read_excel(input_path, sheet = quadrat_sheet)
  swath_raw <- readxl::read_excel(input_path, sheet = swath_sheet) 
  
  #..................Clean sheets to prep for merge..................
  # Clean point_contact dataset 
  point_contact_clean <- point_contact_raw %>% 
    # Remove non-matching columns 
    dplyr::select(-c('number_of_transect_locations', 'percent_cover')) %>% 
    # Rename num of hits to total count 
    dplyr::rename(total_count = number_of_hits) %>% 
    # Create new data collection source column 
    dplyr::mutate(collection_source = "point contact") %>%  
    dplyr::filter(!species_lump %in% c("Rock", "Sand", "Tar", "Blue Green Algae", "Red Crust", "Diatom", "Ceramiales"))
  
  # Clean quadrat dataset 
  quadrat_clean <- quadrat_raw %>% 
    # Remove non-matching columns 
    #dplyr::select(-c('number_of_quadrats_sampled', 'total_area_sampled_m2', 'density_per_m2')) %>% 
    # Create new data collection source column 
    dplyr::mutate(collection_source = "quadrat") %>% 
    dplyr::filter(island == "Mainland") %>% 
    dplyr::filter(!species_lump %in% c("Rock", "Sand", "Tar", "Blue Green Algae", "Red Crust", "Diatom", "Ceramiales"))
  
  # Clean swath dataset 
  swath_clean <- swath_raw %>% 
    # Remove non-matching columns 
    #dplyr::select(-c('number_of_transects_sampled', 'est_swath_area_searched_m2',  'density_per_m2')) %>% 
    # Create new data collection source column 
    dplyr::mutate(collection_source = "swath") %>% 
    dplyr::filter(island == "Mainland") %>% 
    dplyr::filter(!species_lump %in% c("Rock", "Sand", "Tar", "Blue Green Algae", "Red Crust", "Diatom", "Ceramiales"))
  
  #....................Merge datasets together.....................
  clean_biodiv <- dplyr::bind_rows(point_contact_clean, quadrat_clean, swath_clean)
  #clean_biodiv <- dplyr::bind_rows(quadrat_clean, swath_clean)
  
  # Return merged dataframe 
  return(clean_biodiv)
}
