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
  ) %>%
  filter(
    state_province == "California",
  ) #%>%
  #filter(species_lump == "Roperia poulsoni")

# Step 1: Prepare the data
northern_cum_den <- cum_den_df(biodiv_distances) %>%
  filter(species_lump %in% northern_dangermond_species_list)
  #filter(species_lump == "Roperia poulsoni")

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
  geom_vline(xintercept = 0.05, linetype = "dashed") +
  labs(
    title = "Northern Range Edge Species Cumulative Density (GAM)",
    y = "Coastline (km)",
    x = "Normalized Cumulative Density"
  )


library(dplyr)
library(tidyr)
library(purrr)

north_df <- north_pred %>%
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


# Step 5: Model the trend of range boundaries over time
# You can use a simple linear model or another GAM

# Linear model approach
north_boundary_model <- lm(north_boundary ~ year_floor, data = north_df)
south_boundary_model <- lm(south_boundary ~ year_floor, data = north_df)

# Or GAM approach for non-linear trends
north_boundary_lm <- lm(north_boundary ~ year_floor, data = north_df)
# Specify a lower k value (basis dimension)
north_boundary_gam <- gam(north_boundary ~ s(year_floor, k = 5), data = north_df)
south_boundary_gam <- gam(south_boundary ~ s(year_floor, k = 5), data = north_df)

# Step 6: Visualize the trends
ggplot(north_df, aes(x = year_floor)) +
  geom_point(aes(y = north_boundary), color = "red") +
  geom_line(aes(y = predict(north_boundary_model)), color = "red") +
  geom_line(aes(y = predict(south_boundary_model)), color = "black") +
  labs(x = "Year", y = "Coastline position (m)",
       title = "Northern range boundary trends over time")


print(north_df %>% arrange(year_floor) %>% select(year_bin, year_floor))
cat("Number of unique years:", length(unique(north_df$year_floor)), "\n")

# Add midpoint year and range size calculation
north_df <- north_df %>%
  mutate(
    # Extract year range (e.g., "2010-2014") and calculate midpoint
    year_mid = year_floor + 2,
    # Calculate range size
    range_size = north_boundary - south_boundary
  )

# 2. EXPLORATORY DATA VISUALIZATION
# Plot the raw data points to understand the pattern
boundary_plot <- ggplot(north_df, aes(x = year_floor)) +
  geom_point(aes(y = north_boundary/1000), color = "red", size = 3) +
  geom_point(aes(y = south_boundary/1000), color = "blue", size = 3) +
  labs(
    x = "Year bin start", 
    y = "Coastline position (km)",
    title = "Range Boundaries Over Time (5-year bins)"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(north_df$year_floor)) +
  theme(legend.position = "bottom")

print(boundary_plot)

# 3. FIT LINEAR MODELS
# For northern boundary
north_model <- lm(north_boundary ~ year_floor, data = north_df)

# For prediction
north_pred <- data.frame(
  year_floor = seq(min(north_df$year_floor), max(north_df$year_floor), by = 1)
)
north_pred$predicted <- predict(north_model, newdata = north_pred)

# For southern boundary
south_model <- lm(south_boundary ~ year_floor, data = north_df)

south_pred <- data.frame(
  year_floor = seq(min(north_df$year_floor), max(north_df$year_floor), by = 1)
)
south_pred$predicted <- predict(south_model, newdata = south_pred)

# For range size
range_model <- lm(range_size ~ year_floor, data = north_df)

range_pred <- data.frame(
  year_floor = seq(min(north_df$year_floor), max(north_df$year_floor), by = 1)
)
range_pred$predicted <- predict(range_model, newdata = range_pred)

# 4. MODEL EVALUATION
# Create tidy summaries of model results
north_summary <- tidy(north_model, conf.int = TRUE)
south_summary <- tidy(south_model, conf.int = TRUE)
range_summary <- tidy(range_model, conf.int = TRUE)

# Print summary tables
cat("Northern Boundary Model Summary:\n")
print(north_summary)
cat("R-squared:", summary(north_model)$r.squared, "\n")
cat("p-value for year effect:", summary(north_model)$coefficients[2, 4], "\n\n")

cat("Southern Boundary Model Summary:\n")
print(south_summary)
cat("R-squared:", summary(south_model)$r.squared, "\n")
cat("p-value for year effect:", summary(south_model)$coefficients[2, 4], "\n\n")

cat("Range Size Model Summary:\n")
print(range_summary)
cat("R-squared:", summary(range_model)$r.squared, "\n")
cat("p-value for year effect:", summary(range_model)$coefficients[2, 4], "\n\n")

# 5. CALCULATE SHIFT RATES AND SIGNIFICANCE
# Northern boundary
north_rate_km_per_year <- coef(north_model)[2]/1000  # Convert to km/year
north_rate_conf <- confint(north_model)[2,]/1000
north_rate_text <- sprintf("Rate: %.2f km/year (95%% CI: %.2f to %.2f)", 
                       north_rate_km_per_year, north_rate_conf[1], north_rate_conf[2])
north_p_value <- summary(north_model)$coefficients[2,4]
north_sig_text <- ifelse(north_p_value < 0.05, 
                       paste("p =", format(north_p_value, digits=3), "✓"), 
                       paste("p =", format(north_p_value, digits=3), "✗"))

# Southern boundary
south_rate_km_per_year <- coef(south_model)[2]/1000  # Convert to km/year
south_rate_conf <- confint(south_model)[2,]/1000
south_rate_text <- sprintf("Rate: %.2f km/year (95%% CI: %.2f to %.2f)",
                       south_rate_km_per_year, south_rate_conf[1], south_rate_conf[2])
south_p_value <- summary(south_model)$coefficients[2,4]
south_sig_text <- ifelse(south_p_value < 0.05, 
                       paste("p =", format(south_p_value, digits=3), "✓"), 
                       paste("p =", format(south_p_value, digits=3), "✗"))

# Range size
range_rate_km_per_year <- coef(range_model)[2]/1000  # Convert to km/year
range_rate_conf <- confint(range_model)[2,]/1000
range_rate_text <- sprintf("Rate: %.2f km/year (95%% CI: %.2f to %.2f)",
                       range_rate_km_per_year, range_rate_conf[1], range_rate_conf[2])
range_p_value <- summary(range_model)$coefficients[2,4]
range_sig_text <- ifelse(range_p_value < 0.05, 
                       paste("p =", format(range_p_value, digits=3), "✓"), 
                       paste("p =", format(range_p_value, digits=3), "✗"))

# 6. CREATE VISUALIZATIONS
# Northern boundary plot
north_plot <- ggplot() +
  geom_point(data = north_df, aes(x = year_floor, y = north_boundary/1000), color = "red", size = 3) +
  geom_line(data = north_pred, aes(x = year_floor, y = predicted/1000), 
            color = "darkred", linewidth = 1) +
  geom_ribbon(data = north_pred, 
             aes(x = year_floor, 
                 ymin = (predicted - 1.96*summary(north_model)$sigma)/1000,
                 ymax = (predicted + 1.96*summary(north_model)$sigma)/1000),
             fill = "red", alpha = 0.2) +
  labs(
    x = "Year bin start", 
    y = "Northern boundary (km)",
    title = "Northern Range Edge Over Time",
    subtitle = paste(north_rate_text, north_sig_text, sep = " | ")
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(north_df$year_floor))

# Southern boundary plot
south_plot <- ggplot() +
  geom_point(data = north_df, aes(x = year_floor, y = south_boundary/1000), color = "blue", size = 3) +
  geom_line(data = south_pred, aes(x = year_floor, y = predicted/1000), 
            color = "darkblue", linewidth = 1) +
  geom_ribbon(data = south_pred, 
             aes(x = year_floor, 
                 ymin = (predicted - 1.96*summary(south_model)$sigma)/1000,
                 ymax = (predicted + 1.96*summary(south_model)$sigma)/1000),
             fill = "blue", alpha = 0.2) +
  labs(
    x = "Year bin start", 
    y = "Southern boundary (km)",
    title = "Southern Range Edge Over Time",
    subtitle = paste(south_rate_text, south_sig_text, sep = " | ")
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(north_df$year_floor))

# Range size plot
range_plot <- ggplot() +
  geom_point(data = north_df, aes(x = year_floor, y = range_size/1000), color = "purple", size = 3) +
  geom_line(data = range_pred, aes(x = year_floor, y = predicted/1000), 
            color = "darkviolet", linewidth = 1) +
  geom_ribbon(data = range_pred, 
             aes(x = year_floor, 
                 ymin = (predicted - 1.96*summary(range_model)$sigma)/1000,
                 ymax = (predicted + 1.96*summary(range_model)$sigma)/1000),
             fill = "purple", alpha = 0.2) +
  labs(
    x = "Year bin start", 
    y = "Range size (km)",
    title = "Species Range Size Over Time",
    subtitle = paste(range_rate_text, range_sig_text, sep = " | ")
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(north_df$year_floor))

# Combined range dynamics plot
combined_plot <- ggplot() +
  geom_ribbon(data = north_pred, 
             aes(x = year_floor, 
                 ymin = (predict(south_model, newdata = north_pred))/1000,
                 ymax = (predict(north_model, newdata = north_pred))/1000),
             fill = "lightgreen", alpha = 0.3) +
  geom_line(data = north_pred, aes(x = year_floor, y = predicted/1000), 
            color = "darkred", linewidth = 1) +
  geom_line(data = south_pred, aes(x = year_floor, y = predicted/1000), 
            color = "darkblue", linewidth = 1) +
  geom_point(data = north_df, aes(x = year_floor, y = north_boundary/1000), color = "red", size = 3) +
  geom_point(data = north_df, aes(x = year_floor, y = south_boundary/1000), color = "blue", size = 3) +
  labs(
    x = "Year bin start", 
    y = "Coastline position (km)",
    title = "Species Range Dynamics Over Time",
    subtitle = sprintf("Range expanding at %.2f km/year | Northern edge: %.2f km/year | Southern edge: %.2f km/year", 
                       range_rate_km_per_year, north_rate_km_per_year, south_rate_km_per_year)
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(north_df$year_floor))

# Arrange plots
grid.arrange(combined_plot, grid.arrange(north_plot, south_plot, range_plot, ncol = 1), ncol = 2, widths = c(1.5, 1))

# 7. SAVE RESULTS
results_df <- data.frame(
  metric = c("Northern boundary", "Southern boundary", "Range size"),
  shift_rate_km_per_year = c(north_rate_km_per_year, south_rate_km_per_year, range_rate_km_per_year),
  confidence_interval_lower = c(north_rate_conf[1], south_rate_conf[1], range_rate_conf[1]),
  confidence_interval_upper = c(north_rate_conf[2], south_rate_conf[2], range_rate_conf[2]),
  p_value = c(north_p_value, south_p_value, range_p_value),
  significant = c(north_p_value < 0.05, south_p_value < 0.05, range_p_value < 0.05),
  r_squared = c(summary(north_model)$r.squared, summary(south_model)$r.squared, summary(range_model)$r.squared)
)

print(results_df)

# 8. ECOLOGICAL INTERPRETATION
cat("\n--- Interpretation of Results ---\n")
if(abs(north_rate_km_per_year) > abs(south_rate_km_per_year)) {
  cat("The northern boundary is shifting more rapidly than the southern boundary.\n")
} else {
  cat("The southern boundary is shifting more rapidly than the northern boundary.\n")
}

if(range_rate_km_per_year > 0) {
  cat("The species range size is expanding over time.\n")
} else {
  cat("The species range size is contracting over time.\n")
}

if(north_rate_km_per_year > 0) {
  cat("The northern boundary is moving northward (poleward shift).\n")
} else {
  cat("The northern boundary is moving southward (equatorward shift).\n")
}

if(south_rate_km_per_year > 0) {
  cat("The southern boundary is moving northward (poleward shift).\n")
} else {
  cat("The southern boundary is moving southward (equatorward shift).\n")
}

# Climate change context
cat("\nIn the context of climate change:\n")
if(north_rate_km_per_year > 0 && south_rate_km_per_year > 0) {
  cat("Both range boundaries are shifting poleward, consistent with a climate warming response.\n")
} else if(north_rate_km_per_year < 0 && south_rate_km_per_year < 0) {
  cat("Both range boundaries are shifting equatorward, which is not typically expected under climate warming.\n")
} else {
  cat("The range boundaries are shifting in different directions, suggesting complex factors beyond simple climate tracking may be involved.\n")
}

# 9. RECOMMENDATIONS FOR FURTHER ANALYSIS
cat("\n--- Recommendations for Further Analysis ---\n")
cat("1. Consider comparing these range shifts to sea temperature data along the coastline\n")
cat("2. Analyze whether these shifts differ from those of non-northern species\n")
cat("3. Examine whether there are taxonomic patterns in range shift rates\n")
cat("4. Investigate if range sizes are correlated with environmental variables\n")