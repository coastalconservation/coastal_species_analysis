
# Load packages 
library(tidyverse)
library(here)
#library(spData)
#library(sf)
#library(tmap)
#library(kableExtra)


# Load data
range_edges <- read_csv(here("data", "processed", "range_list.csv"))

# Pull species with range edges in Dangermond
dangermond_range_edges <- range_edges %>%
  filter(grepl("Dangermond", segment_name)) %>% 
  filter(range_edge_category  %in% c("Northern Range Edge", "Southern Range Edge", "Endemic Presence")) %>% 
  select(segment_name, species_lump, total_counts, percent_years_present, sites_in_buffer, range_edge_category) %>% 
  arrange(desc(segment_name)) %>%
  mutate(percent_years_present = round(percent_years_present, digits=2)) #%>% 
  #filter(total_counts>10)

dangermond_species_list <- unique(dangermond_range_edges$species_lump)
