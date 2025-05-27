
# Load packages 
library(tidyverse)
library(here)

# Load data
range_edges <- read_csv(here("data", "processed", "range_list.csv"))

# Pull species with range edges in Dangermond

# Looking 2 - 8, only northern range edges
# eventually use fill list. and filter out within the function 
northern_dangermond_range_edges <- range_edges %>%
  filter(segment_id %in% (2:8)) %>% 
  filter(range_edge_category == "Northern Range Edge") %>% 
  # select(segment_name, segment_id, species_lump, total_counts, counts_in_buffer,
  #        percent_years_present, sites_in_buffer, range_edge_category) %>% 
  arrange(desc(segment_id)) %>%
  mutate(percent_years_present = round(percent_years_present, digits=2))

northern_dangermond_species_list <- unique(northern_dangermond_range_edges$species_lump)
