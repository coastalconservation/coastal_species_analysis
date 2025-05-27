#| code-fold: true

# Load necessary libraries
library(here) # For building file paths relative to project root
library(readr) # For reading CSV files
library(lubridate) # For working with date/time data (used in sourced scripts)
library(dplyr) # Data manipulation
library(tidyr) # Data tidying
library(purrr) # Functional programming tools
library(ggplot2) # Plotting
library(mgcv) # Generalized Additive Models

# Load data-cleaning and analysis functions
source(here("scripts", "functions", "cumulative_density_dataframe.R"))

# Load processed datasets
processed_data_path <- "/capstone/coastalconservation/data/processed"
marine_path <- read_csv(
  file.path(
    processed_data_path,
    "marine_site_segments.csv"
  )
)
biodiv_df <- read_csv(
  file.path(
    processed_data_path,
    "clean_biodiv_2025.csv"
  )
)

# Filter for California and target species (Roperia poulsoni)
rp_biodiv <- biodiv_df %>%
  left_join(
    marine_path %>%
      select(marine_site_name, coastline_m)
  ) %>%
  filter(
    state_province == "California",
    species_lump == "Roperia poulsoni"
  )

# Step 1: Create cumulative density dataframe
rp_cum_den <- cum_den_df(rp_biodiv)

# Function to add 0 and 1 cumulative density boundaries per group
add_boundaries <- function(df) {
  boundary_pts <- tibble(
    coastline_m = c(min(df$coastline_m), max(df$coastline_m)),
    cum_den_norm = c(0, 1),
    species_lump = first(df$species_lump),
    year_bin = first(df$year_bin),
    year = first(df$year),
    state_province = first(df$state_province)
  )
  bind_rows(boundary_pts, df)
}

# Add boundaries and combine all groups
rp_cum_den_bounded <- rp_cum_den %>%
  group_by(year_bin) %>%
  group_split() %>%
  map_dfr(add_boundaries)

# Step 2: Fit a Generalized Additive Model (GAM)
rp_gam <- gam(
  cum_den_norm ~ s(coastline_m, by = year_bin),
  family = quasibinomial(link = "logit"),
  data = rp_cum_den_bounded
)

# Step 3: Generate model predictions
rp_pred <- expand_grid(
  coastline_m = seq(
    0,
    max(rp_cum_den$coastline_m),
    length.out = 1000
  ),
  year_bin = unique(rp_cum_den$year_bin)
) %>%
  mutate(
    cum_den_norm = predict(
      rp_gam,
      newdata = .,
      type = "response"
    )
  )

# Step 4: Plot raw data and GAM fit
ggplot(rp_cum_den, aes(
  y = coastline_m,
  x = cum_den_norm, color = year_bin
)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(group = year_bin), data = rp_pred) +
  geom_vline(xintercept = c(0.95, 0.05), linetype = "dashed") +
  labs(
    title = "Roperia Poulsoni Cumulative Density (GAM)",
    y = "Coastline (m)",
    x = "Normalized Cumulative Density"
  )

# Step 5: Estimate 5th and 95th percentile boundaries per year_bin
rp_df <- rp_pred %>%
  group_by(year_bin) %>%
  arrange(cum_den_norm, coastline_m, .by_group = TRUE) %>%
  nest() %>%
  mutate(
    boundaries = map(data, ~ {
      df <- .x %>% distinct(cum_den_norm, .keep_all = TRUE)
      list(
        north = approx(df$cum_den_norm, df$coastline_m, xout = 0.95, rule = 2)$y,
        south = approx(df$cum_den_norm, df$coastline_m, xout = 0.05, rule = 2)$y
      )
    }),
    north_boundary = map_dbl(boundaries, "north"),
    south_boundary = map_dbl(boundaries, "south"),
    year_floor = as.integer(substr(year_bin, 1, 4))
  ) %>%
  select(year_bin, north_boundary, south_boundary, year_floor)

# Step 6: Fit linear models to track boundary movement over time
north_boundary_model <- lm(north_boundary ~ year_floor, data = rp_df)
south_boundary_model <- lm(south_boundary ~ year_floor, data = rp_df)

# Predict boundary positions
north_preds <- data.frame(
  year_bin = rp_df$year_bin,
  prediction = predict(north_boundary_model)
)

south_preds <- data.frame(
  year_bin = rp_df$year_bin,
  prediction = predict(south_boundary_model)
)

# Output model slopes
coef(north_boundary_model)[2]
coef(south_boundary_model)[2]

# Step 7: Plot northern and southern boundary shifts over time
ggplot(rp_df, aes(x = year_floor)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = 520859.2599 - 100000, ymax = 520859.2599 + 100000),
    fill = "lightblue", alpha = 0.2, inherit.aes = FALSE
  ) +
  geom_point(aes(y = north_boundary), color = "red") +
  geom_point(aes(y = south_boundary), color = "black") +
  geom_smooth(aes(y = north_boundary), method = "lm", se = FALSE, color = "red") +
  geom_smooth(aes(y = south_boundary), method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Northern and Southern Range Boundaries Over Time",
    y = "Coastline position (m)",
    x = "Year"
  )
