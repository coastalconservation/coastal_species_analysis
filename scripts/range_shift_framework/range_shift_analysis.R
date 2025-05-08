
#| code-fold: true
library(here)
library(readr)
library(lubridate)
library(tidyverse)
library(stats)

## Read data
# Source cleaning function
source(here("scripts", "functions", "clean_biodiv.R"))
source(here("scripts", "functions", "cumulative_density_dataframe.R"))
source(here("scripts", "functions", "range_extent_prediction.R"))
source(here("scripts", "functions", "cumulative_density_graph.R"))

# Load data
species_extent <- read_csv("/capstone/coastalconservation/data/processed/species_extent.csv")
marine_path <- read_csv("/capstone/coastalconservation/data/processed/marine_site_segments.csv")
biodiv_df <- clean_biodiv()

northern_dangermond_range_edges <- species_extent %>% 
  filter(northern_extent_id %in% (2:8))

northern_dangermond_species_list <- northern_dangermond_range_edges$species_lump %>% unique()

biodiv_distances <- biodiv_df %>% 
             left_join(
              marine_path %>%
              select(marine_site_name, coastline_km)
              )

northern_cum_den <- cum_den_df(biodiv_df) %>% 
  filter(species_lump %in% northern_dangermond_species_list) %>% 
  filter(state_province=="California")

north_logit <- glm(cum_den_norm ~ coastline_km * year_bin, binomial(link="logit"), northern_cum_den)

north_pred <- expand_grid(coastline_km = seq(64727.0844, 797950.6234, length.out = 1000),
                          year_bin = northern_cum_den$year_bin %>% unique()
                         ) %>% 
    mutate(
      cum_den_norm = predict(north_logit,
                             newdata = ., type = "response")
    ) 

library(car)
Anova(north_logit)
coef(north_logit)
summary(north_logit)

ggplot(northern_cum_den, aes(x=coastline_km, y=cum_den_norm, color=year_bin)) +
 geom_point() +
  geom_line(aes(group = year_bin), data=north_pred) +
  xlim(64727.0844, 797950.6234) +
  geom_hline(yintercept=.95) +
  labs(title = "Northern Range Edge Species Cumulative Density") 
 