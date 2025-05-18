#| code-fold: true
library(here)
library(readr)
library(lubridate)
library(tidyverse)
library(stats)
library(mgcv)
library(car)

## Read data
# Source cleaning function
source(here("scripts", "functions", "clean_biodiv.R"))
source(here("scripts", "functions", "cumulative_density_dataframe.R"))
source(here("scripts", "functions", "range_extent_prediction.R"))
source(here("scripts", "functions", "cumulative_density_graph.R"))

# Load data
processed_data_path <- "/capstone/coastalconservation/data/processed"
species_extent <- read_csv(file.path(
  processed_data_path,
  "species_extent.csv"
))
marine_path <- read_csv(file.path(
  processed_data_path,
  "marine_site_segments.csv"
))
biodiv_df <- read_csv(file.path(
  processed_data_path,
  "clean_biodiv_2025.csv"
))

northern_dangermond_range_edges <- species_extent %>%
  filter(northern_extent_id %in% (2:8))

northern_dangermond_species_list <- northern_dangermond_range_edges$species_lump %>% unique()

biodiv_distances <- biodiv_df %>%
  left_join(
    marine_path %>%
      select(marine_site_name, coastline_m)
  )

# Step 1: Prepare the data
northern_cum_den <- cum_den_df(biodiv_df) %>%
  filter(species_lump %in% northern_dangermond_species_list) %>%
  filter(state_province == "California")

# Fit GAM model
north_gam <- gam(cum_den_norm ~ s(coastline_m, by = year_bin),
  family = quasibinomial(link = "logit"),
  data = northern_cum_den
)

# Step 3: Generate predictions
north_pred <- expand_grid(
  coastline_m = seq(0, 1800000, length.out = 1000),
  year_bin = unique(northern_cum_den$year_bin)
) %>%
  mutate(
    cum_den_norm = predict(north_gam, newdata = ., type = "response")
  )

# Step 4: Model summary and diagnostics
anova(north_gam, test = "F")
summary(north_gam)
coef(north_gam)

# Step 5: Plot the results
ggplot(northern_cum_den, aes(
  y = coastline_m,
  x = cum_den_norm, color = year_bin
)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(group = year_bin), data = north_pred) +
  #ylim(64727.0844, 797950.6234) +
  geom_vline(xintercept = 0.95, linetype = "dashed") +
  labs(
    title = "Northern Range Edge Species Cumulative Density (GAM)",
    y = "Coastline (km)",
    x = "Normalized Cumulative Density"
  )


north_df <- north_pred %>%
  group_by(year_bin) %>%
  summarise(
    # Northern edge (95th percentile)
    north_boundary = approx(cum_den_norm, coastline_m, xout = 0.95)$y,
    # Southern edge (5th percentile)
    south_boundary = approx(cum_den_norm, coastline_m, xout = 0.05)$y
  ) %>%
  mutate(
    year_floor = as.integer(substring(year_bin, 1, 4))
  )

# Step 5: Model the trend of range boundaries over time
# You can use a simple linear model or another GAM

# Linear model approach
north_boundary_model <- lm(north_boundary ~ year_floor, data = north_df)
south_boundary_model <- lm(south_boundary ~ year_floor, data = north_df)

# Or GAM approach for non-linear trends
north_boundary_lm <- lm(north_boundary ~ year_floor, data = north_df)
south_boundary_gam <- gam(south_boundary ~ s(year_floor), data = north_df)

# Step 6: Visualize the trends
ggplot(north_df, aes(x = year_floor)) +
  geom_point(aes(y = north_boundary), color = "red") +
  geom_line(aes(y = predict(north_boundary_lm)), color = "red") +
  labs(x = "Year", y = "Coastline position (m)",
       title = "Northern range boundary trends over time")
