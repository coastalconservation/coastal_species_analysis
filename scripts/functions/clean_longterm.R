#' Clean and Process MARINe Long-Term Monitoring survey data
#' 
#' @description
#' The cleaning function
#' filters to California mainland sites
#' removes unnecessary substrates
#' and adds a year column 
#'
#' @return A cleaned data frame ready for analysis.
#' @export A cleaned data frame to the "processed" data folder
#' 
#' @note Define input and output file paths
#' 
#' @examples
#' clean_longterm()
#' 
 clean_longterm <- function() {
   
   # Define input and output file paths
   input_path <- "data/raw/MARINe_data/longterm/phototranraw_download.csv"
   output_path <- "data/processed/MARINe_data/longterm/longterm_processed.csv"
   
   # Read raw long-term survey data
   raw_data <- read_csv(input_path)
   
   # Clean raw survey data
   longterm_processed <- raw_data %>%
     filter(state_province == "California" & island == "Mainland") %>%
     filter(!lumping_code %in% c("ROCK", "SAND", "TAR", "NONCRU")) %>%
     mutate(year = year(survey_date))
   
   # Save cleaned long-term survey data to "processed" data folder
   write_csv(longterm_processed, output_path)
   
   return(longterm_processed)
   
 }