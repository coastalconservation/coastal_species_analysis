# Load packages 
library(tidyverse)
library(here)
library(spData)
library(sf)
library(tmap)
library(kableExtra)

# Load data
range_edges <- read_csv(here("data", "processed", "range_list.csv"))

# Pull species with range edges in Dangermond
dangermond_range_edges <- range_edges %>%
  filter(id  %in%  c(5,6),
         range_edge_category  %in% c("Northern Range Edge",
                                     "Southern Range Edge",
                                     "Endemic Presence"
         )
  ) %>% 
  select(id, species_lump, total_counts, percent_years_present, range_edge_category) %>% 
  arrange(desc(id)) %>%
  mutate(id = case_when(
    id == 5 ~ "Southern Dangermond",
    id == 6 ~ "Northern Dangermond",
    TRUE ~ "Other"
    )
  ) %>% 
  mutate(percent_years_present = round(percent_years_present, digits=2)) 

write.csv(dangermond_range_edges, here::here("data", 
                                 "processed",
                                 "dangermond_range_edges.csv"), row.names = FALSE)

dangermond_north_range_edges <- dangermond_range_edges %>% 
  filter(range_edge_category == "Northern Range Edge") 