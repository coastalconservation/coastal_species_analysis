#' Clean and Process MARINe Long-Term Monitoring survey data
#'
#' @description 
#' The function filters raw phototransect data to 
#' include only California mainland sites, 
#' removes unnecessary substrates, 
#' and adds a column for the year.
#' 
#' @param longterm A data frame containing raw MARINe phototransect data.
#'
#' @return A cleaned data frame ready for analysis.
#' @export
#'
#' @examples
#' raw_data <- read_csv("/path/to/file.csv")
#' data_processed <- clean_longterm(raw_data)
#' 
clean_longterm <- function(longterm){
  
  longterm <- longterm %>%
    filter(state_province == "California" & island == "Mainland") %>%
    filter(!lumping_code %in% c("ROCK", "SAND", "TAR", "NONCRU")) %>%
    mutate(year = year(survey_date))
  
  return(longterm)
}

# Read raw phototransect survey data
longterm <- read_csv("/capstone/coastalconservation/data/raw/MARINe_data/longterm/phototranraw_download.csv")

# Apply the cleaning function
longterm_processed <- clean_longterm(longterm)

# Save the cleaned data to the "processed" data folder
write_csv(x = longterm_processed, file = "/capstone/coastalconservation/data/processed/MARINe_data/longterm_processed.csv")