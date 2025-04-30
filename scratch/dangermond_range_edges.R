# Load packages 
library(tidyverse)
library(here)
library(spData)
library(sf)
library(tmap)
library(kableExtra)

# Load data
range_edges <- read_csv(here("data", "processed", "range_list.csv"))

# # Pull species with range edges in Dangermond
dangermond_range_edges <- range_edges %>%
  filter(segment_id  %in%  c(6,7),
         range_edge_category  %in% c("Northern Range Edge",
                                     "Southern Range Edge",
                                     "Endemic Presence"
         )
  ) %>%
  select(segment_name, species_lump, counts_in_buffer, percent_years_present, range_edge_category) %>%
  arrange(desc(segment_name)) %>%
  mutate(percent_years_present = round(percent_years_present, digits=2))
# 
# write.csv(dangermond_range_edges, here::here("data", 
#                                  "processed",
#                                  "dangermond_range_edges.csv"), row.names = FALSE)
# 
# dangermond_north_range_edges <- dangermond_range_edges %>% 
#   filter(range_edge_category == "Northern Range Edge") 

northern_range_species <- range_edges %>% 
  filter(range_edge_category  == "Northern Range Edge") %>% 
  filter(segment_id >= 2) %>% 
  filter(segment_id <= 7) 

dangemond_north_species <- range_edges %>% 
  filter(species_lump %in% c("Agathistoma eiseni","Acanthinucella paucilirata",
                "Aplysia californica", "Haminoea vesicula", "Roperia poulsoni")) %>% 
  filter(segment_id >= 2) %>% 
  filter(segment_id <= 7) %>% 
  select(species_lump, counts_in_buffer, total_counts,
         percent_years_present, segment_id, segment_name)
