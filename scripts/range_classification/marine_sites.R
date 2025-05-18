# Load required libraries
library(tidyverse)
library(here)

# Source the cleaned MARINe data function
source(here::here("scripts", "functions", "clean_biodiv.R"))

# Initialize file paths
raw_data_path <- "/capstone/coastalconservation/data/raw/"
processed_data_path <- "/capstone/coastalconservation/data/processed/"
site_csv_path <- "spatial_data/marine_sites_distance_coast.csv"

marine_site_distance_path <- file.path(raw_data_path, site_csv_path)
ca_segment_path <- file.path(processed_data_path, "ca_segments.csv")

# Read in data
marine_site_distance <- read_csv(marine_site_distance_path)
ca_segments <- read_csv(ca_segment_path)
biodiv_df <- clean_biodiv()

# Filter and join MARINe sites with coastline distance
marine_sites <- biodiv_df %>%
  distinct(marine_site_name, latitude, longitude) %>%
  left_join(
    marine_site_distance %>%
      select(marine_site_name, coastline_m = MEAS),
    by = "marine_site_name"
  )

# Define helper function to assign segment_id based on site latitude
assign_segment_id <- function(site_lat, segments) {
  possible <- segments %>%
    filter(min_latitude <= site_lat)
  if (nrow(possible) > 0) {
    # choose the first/lowest possible match
    return(possible$segment_id[1])
  } else {
    return(NA_real_)
  }
}

# Apply segment assignment
marine_sites <- marine_sites %>%
  rowwise() %>%
  mutate(segment_id = assign_segment_id(
    latitude,
    ca_segments
  )) %>%
  ungroup()

# Join segment names
marine_sites <- marine_sites %>%
  left_join(
    ca_segments %>% select(segment_id, segment_name),
    by = "segment_id"
  )

# View sites
marine_sites %>% View()

# Save marine_sites
write_csv(
  marine_sites,
  file.path(
    processed_data_path,
    "marine_site_segments.csv"
  )
)
