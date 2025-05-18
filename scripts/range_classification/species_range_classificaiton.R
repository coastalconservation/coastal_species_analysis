# Load required libraries
library(tidyverse)
library(here)

# Source the cleaned MARINe data function
source(here::here("scripts", "functions", "clean_biodiv.R"))

# Initialize file paths

processed_data_path <- "/capstone/coastalconservation/data/processed"
marine_site_path <- file.path(processed_data_path, "marine_site_segments.csv")

# Read in data
biodiv_df <- clean_biodiv()
marine_site_segments <- read_csv(marine_site_path)

# Filter to observations that are present
present_observations <- biodiv_df %>%
  # remove non values
  filter(
    !is.na(total_count) | !is.na(number_of_hits)
  ) %>%
  # only work with non zero values
  filter(
    total_count > 0 |
      number_of_hits > 0
  )

# Find southernmost observation site
southern_sites <- present_observations %>%
  group_by(species_lump) %>%
  slice_min(order_by = latitude, n = 1, with_ties = FALSE) %>%
  select(species_lump,
    southern_extent_lat = latitude,
    southern_site_name = marine_site_name
  )

# Find northernmost observation site
northern_sites <- present_observations %>%
  group_by(species_lump) %>%
  slice_max(order_by = latitude, n = 1, with_ties = FALSE) %>%
  select(species_lump,
    northern_extent_lat = latitude,
    northern_site_name = marine_site_name
  )

# Join tables to create extent
species_extent <- southern_sites %>%
  inner_join(northern_sites, by = "species_lump")

# Add coastline distance to each extent
species_extent <- species_extent %>%
  # First join on southern_extent_lat (southern)
  left_join(
    marine_site_segments %>%
      select(
        marine_site_name, segment_id,
        segment_name, coastline_m
      ) %>%
      rename(
        southern_extent_m = coastline_m
      ),
    join_by(southern_site_name == marine_site_name)
  ) %>%
  rename(
    southern_extent_id = segment_id,
    southern_extent_name = segment_name
  ) %>%
  # Second join on northern_extent_lat (northern)
  left_join(
    marine_site_segments %>%
      select(
        marine_site_name, segment_id,
        segment_name, coastline_m
      ) %>%
      rename(
        northern_extent_m = coastline_m
      ),
    join_by(northern_site_name == marine_site_name)
  ) %>%
  rename(
    northern_extent_id = segment_id,
    northern_extent_name = segment_name
  ) %>%
  select(
    species_lump,
    southern_extent_lat, southern_extent_m,
    southern_extent_id, southern_extent_name,
    northern_extent_lat, northern_extent_m,
    northern_extent_id, northern_extent_name
  )

# Write file
species_extent_path <- file.path(processed_data_path, "species_extent.csv")
write_csv(
  species_extent,
  species_extent_path
)
