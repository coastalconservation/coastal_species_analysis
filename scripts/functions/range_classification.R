#' Analyze Species Ranges Along Coastline Segments
#'
#' Calculates species ranges within defined coastline segments using biodiversity data.
#'
#' @param cbs_excel_name Name of the CBS Excel file.
#' @param point_contact_sheet Name of the point contact sheet.
#' @param quadrat_sheet Name of the quadrat sheet.
#' @param swath_sheet Name of the swath sheet.
#'
#' @return A data frame (`range_list`) of species range analysis results per coastline segment, saved to "data/processed/range_list.csv".
#'
#' @details Reads biodiversity data, divides the California coastline into segments,
#' and uses `buffer_classification` (from 'buffer_classification.R') to analyze species
#' presence within each segment. Relies on cleaned biodiversity data from
#' `clean_biodiv.R`


range_classification <- function(cbs_excel_name = 'cbs_data_2025.xlsx',
                                     point_contact_sheet = 'point_contact_summary_layered',
                                     quadrat_sheet = 'quadrat_summary_data',
                                     swath_sheet = 'swath_summary_data') {
    # Load required libraries
    library(tidyverse)
    library(sf)
    library(here)
    
    # Source external R scripts for custom functions
    source(here::here('scripts', 'functions', 'clean_biodiv.R'))
    source(here::here('scripts', 'functions', 'buffer_classification.R'))
    
    # Load coastline segments data, rename columns, convert to spatial format, and add segment ID
    ca_breaks <- read_csv(here('data', 'raw', 'spatial_data', 'ca_segments', 'CA_coast_021425.csv')) %>%
      rename(lat = POINT_Y, long = POINT_X) %>% # Renaming columns for clarity # Convert to sf object with correct CRS
      mutate(segment_id = 1:nrow(.)) # Add unique ID for each coastline segment
    
    # Clean and process biodiversity data using custom function
    biodiv_merge <- clean_biodiv(cbs_excel_name = cbs_excel_name,
                                   point_contact_sheet = point_contact_sheet,
                                   quadrat_sheet = quadrat_sheet,
                                   swath_sheet = swath_sheet)
    
    # Summarize biodiversity data by grouping by site, species, and year, and calculate total counts
    biodiv_total <- biodiv_merge %>%
      group_by(marine_site_name, latitude, longitude, species_lump, year) %>%
      summarise(num_count = sum(total_count, na.rm = TRUE)) %>% # Sum the count of species observed
      mutate(presence = case_when(num_count >= 1 ~ TRUE, TRUE ~ FALSE)) # Determine presence/absence based on num_count
    
    # Sort the latitudes of the coastline segments in ascending order
    coastline_lat <- ca_breaks$lat %>%
      sort(decreasing = FALSE)
    
    segment_names = c("1" = "Baja",
                      "2" = "Chula Vista",
                      "3" = "North County San Diego", 
                      "4" = "Orange County",
                      "5" = "Point Mugu",
                      "6" = "Southern Dangermond",
                      "7" = "Northern Dangermond",
                      "8" = "Morro Bay",
                      "9" = "Big Sur",
                      "10" = "Monterey Bay",
                      "11" = "San Francisco",
                      "12" = "Point Reyes",
                      "13" = "Bodega Bay",
                      "14" = "Fort Bragg",
                      "15" = "Humbolt",
                      "16" = "Eureka",
                      "17" = "Crescent City",
                      "18" = "Pacific North West"
                      )
    
    # Loop over coastline segments, calculate species range, and collect results into a list
    range_list <- map_dfr(1:(length(coastline_lat) - 1), function(i) {
      # 
      buffer_classification(biodiv_total, coastline_lat[i], coastline_lat[i + 1]) %>%
        # Create a range label
        mutate(range_lat = paste(round(coastline_lat[i], 2), 
                                 round(coastline_lat[i + 1], 2), sep = "-"), 
               # Assign segment ID to each result
               segment_id = i, 
               # Assign segment name to each result
               segment_name = segment_names[i]) 
    })
    
    # Return the compiled range list
    return(range_list)
}
