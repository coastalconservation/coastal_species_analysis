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
#' and uses `species_range` (from 'species_range_function.R') to analyze species
#' presence within each segment. Relies on cleaned biodiversity data from
#' `MarineBioClean.R`


analyze_coastline_ranges <- function(cbs_excel_name = 'cbs_data_2025.xlsx',
                                     point_contact_sheet = 'point_contact_summary_layered',
                                     quadrat_sheet = 'quadrat_summary_data',
                                     swath_sheet = 'swath_summary_data') {
    # Load required libraries
    library(tidyverse)
    library(sf)
    library(here)
    
    # Source external R scripts for custom functions
    source(here::here('scripts', 'R', 'MarineBioClean.R'))
    source(here::here('scripts', 'range_classification',
                      'coastline_range_edges', 'species_range_function.R'))
    
    # CA coastal zone boundary
    ca_boundary <- st_read(here('data', 'raw', 'mapping', 'ds990.gdb'))
    
    # Load coastline segments data, rename columns, convert to spatial format, and add segment ID
    ca_breaks <- read_csv(here('data', 'raw', 'mapping', 'CA_coast_021425.csv')) %>%
      rename(lat = POINT_Y, long = POINT_X) %>% # Renaming columns for clarity
      st_as_sf(coords = c("long", "lat"), crs = st_crs(ca_boundary), remove = FALSE) %>% # Convert to sf object with correct CRS
      mutate(segment_id = 1:nrow(.)) # Add unique ID for each coastline segment
    
    # Clean and process biodiversity data using custom function
    biodiv_merge <- MarineBioClean(cbs_excel_name = cbs_excel_name,
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
    
    # Loop over coastline segments, calculate species range, and collect results into a list
    range_list <- map_dfr(1:(length(coastline_lat) - 1), function(i) {
      species_range(biodiv_total, coastline_lat[i], coastline_lat[i + 1]) %>%
        mutate(range_lat = paste(round(coastline_lat[i], 2), round(coastline_lat[i + 1], 2), sep = "-"), # Create a range label
               id = i) # Assign segment ID to each result
    })
    
    write.csv(range_list, here::here("data",
                                     "processed",
                                     "range_list.csv"), row.names = FALSE)
    
    # Return the compiled range list
    return(range_list)
  }