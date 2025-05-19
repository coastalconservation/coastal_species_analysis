#' Plot Annual Range Extent for a Species
#'
#' @description
#' Creates a visualization of a species' range boundaries over time by plotting
#' the latitudinal extent (5th and 95th percentiles) for each year. This function
#' helps track range shifts.
#'
#' @param species_name Character string specifying the species name to analyze
#'   (must match values in the species_lump column of the input data).
#'
#' @return A ggplot object showing the northern (95th percentile, red) and southern
#'   (5th percentile, blue) range boundaries across years, with a reference line
#'   at latitude 34.449 degrees.
#'
#' @details
#' The function calculates range boundaries through these steps:
#' \itemize{
#'   \item Retrieves cumulative density data for the specified species
#'   \item Fits a logistic regression model of normalized cumulative density by latitude and year
#'   \item Predicts cumulative density across latitudes (32-36°) for years 2000-2024
#'   \item For each year, identifies the latitudes corresponding to 5% and 95% of cumulative density
#'   \item Visualizes these boundaries with southern edge in blue and northern edge in red
#' }
#'
#' @examples
#' \dontrun{
#' # Plot range extent for Roperia
#' range_extent_graph("Roperia poulsoni")
#'
#' # Save the plot
#' butterfly_range <- range_extent_graph("Roperia poulsoni")
#' ggsave("tiger_swallowtail_range.png", butterfly_range, width = 8, height = 6)
#' }
#'
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_hline labs
#' @importFrom tidyr expand_grid
#' @importFrom stats glm binomial predict approx
#'
#' @seealso
#' \code{\link{cum_den_df}} for the function that prepares cumulative density data
#' \code{\link{cumulative_den_graph}} for related visualization of the full density curves
#'
#' @export
#'
source(here::here("scripts", "functions", "clean_biodiv.R"))

range_trend <- function(species_name, biodiv_df = clean_biodiv()) {
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
    ),
    show_col_types = FALSE
  )
  biodiv_df <- read_csv(
    file.path(
      processed_data_path,
      "clean_biodiv_2025.csv"
    ),
    show_col_types = FALSE
  )

  # Filter for California and target species
  species_biodiv <- biodiv_df %>%
    left_join(
      marine_path %>%
        select(marine_site_name, coastline_m),
      by = join_by(marine_site_name)
    ) %>%
    filter(
      state_province == "California",
      species_lump == species_name
    )
  # Step 1: Create cumulative density dataframe
  species_cum_den <- cum_den_df(species_biodiv)

  # Exit early if no data
  # if (nrow(species_cum_den) == 0 || all(is.na(species_cum_den$cum_den_norm))) {
  #   return(list(
  #     n_bound_pos_trend = NA,
  #     s_bound_pos_trend = NA
  #   ))
  # }
  if (nrow(species_cum_den) < 10 || all(species_cum_den$cum_den == 0, na.rm = TRUE)) {
    warning(paste("Insufficient or all-zero data for:", species_name))
    return(list(
      n_bound_pos_trend = NA,
      s_bound_pos_trend = NA
    ))
  }


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
  species_cum_den_bounded <- species_cum_den %>%
    group_by(year_bin) %>%
    group_split() %>%
    map_dfr(add_boundaries)

  # Step 2: Fit a Generalized Additive Model (GAM)
  species_gam <- gam(
    cum_den_norm ~ s(coastline_m, by = year_bin),
    family = quasibinomial(link = "logit"),
    data = species_cum_den_bounded
  )

  # Step 3: Generate model predictions
  species_pred <- expand_grid(
    coastline_m = seq(
      0,
      max(species_cum_den$coastline_m),
      length.out = 1000
    ),
    year_bin = unique(species_cum_den$year_bin)
  ) %>%
    mutate(
      cum_den_norm = predict(
        species_gam,
        newdata = .,
        type = "response"
      )
    )

  # Step 5: Estimate 5th and 95th percentile boundaries per year_bin
  species_df <- species_pred %>%
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
  north_boundary_model <- lm(north_boundary ~ year_floor, data = species_df)
  south_boundary_model <- lm(south_boundary ~ year_floor, data = species_df)

  return(list(
    n_bound_pos_trend = coef(north_boundary_model)[2] %>% unname() > 0,
    s_bound_pos_trend = coef(south_boundary_model)[2] %>% unname() > 0
  ))
}


# # Predict boundary positions
# north_preds <- data.frame(
#   year_bin = species_df$year_bin,
#   prediction = predict(north_boundary_model)
# )

# south_preds <- data.frame(
#   year_bin = species_df$year_bin,
#   prediction = predict(south_boundary_model)
# )

# # Step 4: Plot raw data and GAM fit
# ggplot(species_cum_den, aes(
#   y = coastline_m,
#   x = cum_den_norm, color = year_bin
# )) +
#   geom_point(alpha = 0.6) +
#   geom_line(aes(group = year_bin), data = species_pred) +
#   geom_vline(xintercept = c(0.95, 0.05), linetype = "dashed") +
#   labs(
#     title = paste(species_name, "Cumulative Density (GAM)"),
#     y = "Coastline (m)",
#     x = "Normalized Cumulative Density"
#   )


# # Step 7: Plot northern and southern boundary shifts over time
# ggplot(species_df, aes(x = year_floor)) +
#   geom_rect(
#     aes(xmin = -Inf, xmax = Inf, ymin = 520859.2599 - 100000, ymax = 520859.2599 + 100000),
#     fill = "lightblue", alpha = 0.2, inherit.aes = FALSE
#   ) +
#   geom_point(aes(y = north_boundary), color = "red") +
#   geom_point(aes(y = south_boundary), color = "black") +
#   geom_smooth(aes(y = north_boundary), method = "lm", se = FALSE, color = "red") +
#   geom_smooth(aes(y = south_boundary), method = "lm", se = FALSE, color = "black") +
#   labs(
#     title = "Northern and Southern Range Boundaries Over Time",
#     y = "Coastline position (m)",
#     x = "Year"
#   )
