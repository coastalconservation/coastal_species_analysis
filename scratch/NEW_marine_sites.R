library(tidyverse)
library(dplyr)

source(here::here("scripts", "functions", "clean_biodiv.R"))

biodiv_df <- clean_biodiv()

marine_sites <- biodiv_df %>% 
  filter(state_province == "California") %>%
  distinct(marine_site_name, latitude, longitude)

ca_segments <- read_csv("/capstone/coastalconservation/data/processed/ca_segments.csv")

# Initialize a vector to store the matching segment_id
matched_segment_id <- numeric(nrow(marine_sites))

# Loop through each marine site
for (i in seq_len(nrow(marine_sites))) {
  site_lat <- marine_sites$latitude[i]
  
  # Find the largest segment latitude less than or equal to site latitude
  possible_segments <- ca_segments[ca_segments$min_latitude <= site_lat, ]
  
  if (nrow(possible_segments) > 0) {
    matched_segment_id[i] <- possible_segments$segment_id[1]  # take the first match
  } else {
    matched_segment_id[i] <- NA  # no matching segment below
  }
}

marine_sites$segment_id <- matched_segment_id

# Map segment_id to segment_name (handle NA safely)
marine_sites$segment_name <- ifelse(
  is.na(marine_sites$segment_id),
  NA,
  segment_names[as.character(marine_sites$segment_id)]
)

marine_sites <- marine_sites %>% 
  left_join(ca_segments %>% 
                select (segment_id, segment_name),
      join_by(segment_id)
  )

write.csv(marine_sites, "/capstone/coastalconservation/data/processed/marine_site_segments.csv")
