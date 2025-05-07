library(tidyverse)

biodiv_df <- clean_biodiv()
marine_path <- "/capstone/coastalconservation/data/processed/marine_site_segments.csv"
marine_site_segments <- read_csv(marine_path)

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

species_extent <- yearly_species_extent %>%
  group_by(species_lump) %>%
  summarise(
    min_extent = min(min_lat),
    max_extent = max(max_lat)
  ) %>% 
  ungroup()

species_extent <- species_extent %>%
  # First join on min_extent (southern)
  left_join(
    marine_site_segments %>%
      select(latitude, segment_id, segment_name, coastline_km) %>%
      rename(
        southern_extent_km = coastline_km
      ),
    join_by(min_extent == latitude)
  ) %>%
  rename(
    southern_extent_id = segment_id,
    southern_extent_name = segment_name
  ) %>%
  
  # Second join on max_extent (northern)
  left_join(
    marine_site_segments %>%
      select(latitude, segment_id, segment_name, coastline_km) %>%
      rename(
        northern_extent_km = coastline_km
      ),
    join_by(max_extent == latitude)
  ) %>%
  rename(
    northern_extent_id = segment_id,
    northern_extent_name = segment_name
  )


write.csv(species_extent, "/capstone/coastalconservation/data/processed/species_extent.csv")

com_names_test <- fb_tbl("species") %>% 
  mutate(
    species_lump = paste(Genus, Species, sep = " ")
  ) %>%
  select(
    species_lump,
    FBname
  )

species_extent %>%
  left_join(com_names_test,
            join_by(species_lump)
  ) %>%
  View()
