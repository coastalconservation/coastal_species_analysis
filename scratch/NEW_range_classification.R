yearly_species_extent <- biodiv_df %>% 
  # remove non values
  filter(
    !is.na(total_count)|
    !is.na(number_of_hits)
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
  )
