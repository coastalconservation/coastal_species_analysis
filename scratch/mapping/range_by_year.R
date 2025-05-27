biodiv_df <- clean_biodiv()

processed_data_path <- "/capstone/coastalconservation/data/processed"

marine_site_path <- file.path(processed_data_path, "marine_site_segments.csv")
marine_site_segments <- read_csv(marine_site_path)

yearly_species_extent <- biodiv_df %>%
  # remove non values
  filter(
    !is.na(total_count) | !is.na(number_of_hits)
  ) %>%
  # only work with non zero values
  filter(
    total_count > 0 |
    number_of_hits > 0
  ) %>%
  group_by(species_lump, year) %>%
  summarise(
    min_lat = min(latitude),
    max_lat = max(latitude)
  ) %>%
  ungroup()