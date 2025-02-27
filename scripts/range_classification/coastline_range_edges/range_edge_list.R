# Load required libraries
library(tidyverse)  # Includes core packages for data manipulation (e.g., dplyr, ggplot2)
library(sf)         # For spatial data handling and manipulation
library(here)       # For file path management, ensuring portability across systems

# Source external R scripts for custom functions
source(here::here('scripts', 'R', 'MarineBioClean.R'))  # Custom function for cleaning biodiversity data
source(here::here('scripts', 'range_classification', 
                  'coastline_range_edges', 'species_range_function.R'))  # Custom function for analyzing species range

# CA coastal zone boundary 
ca_boundary <- st_read(here('data', 'raw', 'mapping', 'ds990.gdb'))

# Load coastline segments data, rename columns, convert to spatial format, and add segment ID
ca_breaks <- read_csv(here('data', 'raw', 'mapping', 'CA_coast_021425.csv')) %>% 
  rename(lat = POINT_Y, long = POINT_X) %>%  # Renaming columns for clarity
  st_as_sf(coords = c("long", "lat"), crs = st_crs(ca_boundary), remove = FALSE) %>%  # Convert to sf object with correct CRS
  mutate(segment_id = 1:nrow(.))  # Add unique ID for each coastline segment

# Clean and process biodiversity data using custom function
biodiv_merge <- MarineBioClean(cbs_excel_name = 'cbs_data_2025.xlsx', 
                               point_contact_sheet = 'point_contact_summary_layered',
                               quadrat_sheet = 'quadrat_summary_data',
                               swath_sheet = 'swath_summary_data')

# Summarize biodiversity data by grouping by site, species, and year, and calculate total counts
biodiv_total <- biodiv_merge %>% 
  group_by(marine_site_name, latitude, longitude, species_lump, year) %>% 
  summarise(num_count = sum(total_count)) %>%  # Sum the count of species observed
  mutate(presence = case_when(num_count >= 1 ~ TRUE, TRUE ~ FALSE))  # Determine presence/absence based on num_count

# Sort the latitudes of the coastline segments in ascending order
# Add one chunk above one below then drop it
coastline_lat <- ca_breaks$lat%>% 
  sort(decreasing = FALSE) 

# Loop over coastline segments, calculate species range, and collect results into a list
range_list <- map_dfr(1:(length(coastline_lat) - 1), function(i) {
  species_range(biodiv_total, coastline_lat[i], coastline_lat[i + 1]) %>%
    mutate(range_lat = paste(round(coastline_lat[i], 2), round(coastline_lat[i + 1], 2), sep = "-"),  # Create a range label
           id = i)  # Assign segment ID to each result
})

write.csv(range_list, here::here("data", 
                                 "processed",
                                 "range_list.csv"), row.names = FALSE)
# Return the compiled range list
return(range_list)
