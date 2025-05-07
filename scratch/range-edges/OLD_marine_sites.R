# Load required libraries
library(tidyverse)  # Includes core packages for data manipulation (e.g., dplyr, ggplot2)
library(sf)         # For spatial data handling and manipulation
library(here)       # For file path management, ensuring portability across systems
library(tmap)
library(lwgeom)
library(rpostgis)

# Source external R scripts for custom functions
source(here::here('scripts', 'functions', 'clean_biodiv.R'))

# # UTM Zone 10N for California coast
CA_coastline <- read_sf(here('data', 'raw', 'spatial_data',
                             'CZB', 'Statewide_CoastalZoneBoundary_Cadastral.shp')) %>% 
  st_transform(crs=32610)

biodiv_df <- read_csv(here("data","processed", "MARINe_full.csv"))

marine_sites <- biodiv_df %>%
  filter(latitude >= 32.5, latitude <= 42) %>%   
  distinct(marine_site_name, latitude, longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%  
  st_transform(crs = 32610) 

# 3. Snap marine sites to the line
marine_sites_snapped <- marine_sites %>%
  mutate(nearest_id = st_nearest_feature(geometry, CA_coastline)) %>%
  rowwise() %>%
  mutate(
    snapped_geometry = st_nearest_points(geometry, CA_coastline[nearest_id, ]) %>%
      st_cast("POINT") %>%
      {.[2]}
  ) %>%
  ungroup() %>%
  st_as_sf() %>%
  mutate(geometry = snapped_geometry) %>%
  select(marine_site_name, geometry)

tm_shape(CA_coastline) +
  tm_lines() +
  tm_shape(marine_sites_snapped) +
  tm_dots()+#col = "distance_along_coast", palette = "viridis", size = 0.2) +
  tm_text("marine_site_name", size = 0.6, just = "left") +
  tm_layout(legend.outside = TRUE)

#   
# > st_distance(marine_sites, marine_sites_snapped, by_element = TRUE)/1000
#tm_shape(marine_sites_snapped) +
  #tm_dots(fill = "red") +
  #tm_shape(marine_sites) +
  #tm_dots(fill = "blue") +
  tm_shape(single_line_coastline) +
  tm_lines() #+
  #tm_shape(marine_sites_distances) +
  #tm_dots(col='distance_along_coastline')
  # tm_shape(southern_point) +
  # tm_dots(fill="green")
