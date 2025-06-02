# ---------------------------------------------------------------------------

# Description: This script names the 100 km coastal segments, it extracts the northern and southern boundaries of each segement

# ---------------------------------------------------------------------------

# Load required libraries
library(tidyverse)

# Initialize file paths
raw_data_path <- "/capstone/coastalconservation/data/raw"
coast_segments_path <- "spatial_data/ca_segments/CA_coast_021425.csv"
segment_path <- file.path(raw_data_path, coast_segments_path)

# Load and clean data
ca_segments <- read_csv(segment_path) %>%
  rename(
    # Rename columns
    segment_id = OID_,
    latitude = POINT_Y
  ) %>%
  # Order from south to north
  mutate(
    segment_id = rev(segment_id)
  ) %>%
  select(segment_id, latitude)

ca_segments <- ca_segments %>%
  # Find min and max latitude for each segment
  rename(
    min_latitude = latitude
  ) %>%
  mutate(
    max_latitude = lead(min_latitude, order_by = segment_id)
  ) %>%
  # Drop unused segment
  filter(segment_id != 19)

# Declare names for each segment
segment_names <- c(
  "1" = "Baja",
  "2" = "Chula Vista",
  "3" = "North County/San Diego",
  "4" = "LA/Orange County",
  "5" = "Point Mugu",
  "6" = "Southern Point Conception",
  "7" = "Northern Point Conception",
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

# Apply segment names to each row
ca_segments$segment_name <- segment_names[as.character(ca_segments$segment_id)]

# Write data
processed_data_path <- "/capstone/coastalconservation/data/processed"
write_csv(
  ca_segments,
  file.path(processed_data_path, "ca_segments.csv")
)
