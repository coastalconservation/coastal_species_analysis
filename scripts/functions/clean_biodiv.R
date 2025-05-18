#' Clean MARINe Coastal Biodiversity Surveys Data
#'
#' @description
#' This function takes the MARINe CBS Excel file name
#' and merges key sheets for presence-absence analysis.
#' @param cbs_excel_name The filename of the MARINe CBS Excel file
#' (e.g., "cbs_data_2025.xlsx")
#' @param point_contact_sheet Exact name of the point contact summary sheet
#' @param quadrat_sheet Exact name of the quadrat summary sheet
#' @param swath_sheet Exact name of the swath summary sheet
#'
#' @return A cleaned, combined dataframe
#' @export
#'
#' @examples
#' clean_biodiv("cbs_data_2025.xlsx")
clean_biodiv <- function(cbs_excel_name = "cbs_data_2025.xlsx",
                         point_contact_sheet = "point_contact_summary_layered",
                         quadrat_sheet = "quadrat_summary_data",
                         swath_sheet = "swath_summary_data") {
  # .........................Create full path.........................
  raw_data_path <- "/capstone/coastalconservation/data/raw"
  biodiversity_data_path <- "MARINe_data/biodiversity"
  input_path <- file.path(raw_data_path, biodiversity_data_path, cbs_excel_name)

  # .........................Read in sheets..........................
  point_contact_raw <- readxl::read_excel(
    input_path,
    sheet = point_contact_sheet
  )

  quadrat_raw <- readxl::read_excel(
    input_path,
    sheet = quadrat_sheet
  )

  swath_raw <- readxl::read_excel(
    input_path,
    sheet = swath_sheet
  )

  # ..................Clean sheets to prep for merge..................
  # Clean point_contact dataset
  point_contact_clean <- point_contact_raw %>%
    # Create new data collection source column
    dplyr::mutate(collection_source = "point contact")

  # Clean quadrat dataset
  quadrat_clean <- quadrat_raw %>%
    # Create new data collection source column
    dplyr::mutate(collection_source = "quadrat")

  # Clean swath dataset
  swath_clean <- swath_raw %>%
    # Create new data collection source column
    dplyr::mutate(collection_source = "swath")

  # ....................Merge datasets together.....................
  clean_biodiv <- dplyr::bind_rows(
    point_contact_clean, quadrat_clean, swath_clean
  ) %>%
    # Filter out island data
    dplyr::filter(island == "Mainland") %>%
    # Filter out unwanted taxa
    dplyr::filter(
      !species_lump %in% c(
        "Rock", "Sand", "Tar", "Blue Green Algae",
        "Red Crust", "Diatom", "Ceramiales"
      )
    )

  # Return merged dataframe
  return(clean_biodiv)
}
