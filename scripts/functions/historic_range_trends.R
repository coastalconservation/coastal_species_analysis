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
# source(here("scripts", "functions", "cumulative_density_dataframe.R"))

gam_predict <- function(species_name, biodiv_df) {
  # Filter for California and target species
  species_biodiv <- biodiv_df %>%
    left_join(
      marine_sites %>%
        select(marine_site_name, coastline_m),
      by = join_by(marine_site_name)
    ) %>%
    filter(
      state_province == "California",
      species_lump == species_name
    )

  # Step 1: Create cumulative density dataframe
  species_cum_den <- cum_den_df(species_biodiv)

  # Step 2: Split by year_bin
  species_cum_den_split <- species_cum_den %>%
    group_by(year_bin) %>%
    group_split()

  # Step 3: Fit models per year_bin with error handling
  species_pred <- map_dfr(species_cum_den_split, function(data_chunk) {
    year_bin_val <- unique(data_chunk$year_bin)

    # Try to fit GAM
    model <- tryCatch(
      gam(cum_den_norm ~ s(coastline_m), data = data_chunk),
      error = function(e) {
        warning(paste("GAM fitting failed for year bin:", year_bin_val))
        return(NULL)
      }
    )

    if (is.null(model)) {
      # Return placeholder with NAs if GAM fails
      return(tibble(
        year_bin = year_bin_val,
        coastline_m = NA,
        cum_den_norm = NA,
        fit = NA,
        se = NA,
        lower = NA,
        upper = NA
      ))
    }

    # Create prediction grid
    coastline_seq <- seq(
      min(data_chunk$coastline_m, na.rm = TRUE),
      max(data_chunk$coastline_m, na.rm = TRUE),
      length.out = 200
    )

    pred_df <- tibble(
      year_bin = year_bin_val,
      coastline_m = coastline_seq
    )

    # Predict GAM values
    pred_vals <- predict(model, newdata = pred_df, type = "link", se.fit = TRUE)
    pred_df <- pred_df %>%
      mutate(
        cum_den_norm = predict(model, newdata = pred_df),
        cum_den_norm = pmin(pmax(cum_den_norm, 0), 1), # Clamp between 0 and 1
        fit = pred_vals$fit,
        se = pred_vals$se.fit,
        lower = pmax(0, fit - 1.96 * se),
        upper = pmin(1, fit + 1.96 * se)
      )

    return(pred_df)
  })

  return(species_pred)
}



range_trend <- function(species_name, biodiv_df = clean_biodiv()) {
  species_pred <- gam_predict(species_name, biodiv_df)

  # Safe interpolation function
  safe_approx <- function(x, y, xout) {
    # Remove NAs and ensure we have valid data
    valid_idx <- !is.na(x) & !is.na(y)
    if (sum(valid_idx) < 2) {
      return(NA)
    }

    x <- x[valid_idx]
    y <- y[valid_idx]

    dedup <- tibble(x = x, y = y) %>%
      arrange(x) %>%
      distinct(x, .keep_all = TRUE)

    if (nrow(dedup) < 2) {
      return(NA)
    }

    approx(dedup$x, dedup$y, xout = xout, rule = 2)$y
  }


  # FIXED: Correct boundary definitions and variable name
  species_extent_df <- species_pred %>%
    group_by(year_bin) %>%
    summarise(
      year_floor = as.integer(substr(first(year_bin), 1, 4)),
      # FIXED: North boundary should be at higher coastline values (95th percentile)
      # South boundary should be at lower coastline values (5th percentile)
      north_boundary = safe_approx(cum_den_norm, coastline_m, xout = 0.95),
      south_boundary = safe_approx(cum_den_norm, coastline_m, xout = 0.05),
      .groups = "drop"
    ) %>%
    select(year_bin, north_boundary, south_boundary, year_floor)

  north_data <- species_extent_df %>%
    filter(!is.na(north_boundary))

  south_data <- species_extent_df %>%
    filter(!is.na(south_boundary))

  north_boundary_model <- if (nrow(north_data) >= 2) {
    lm(north_boundary ~ year_floor, data = north_data)
  } else {
    NULL
  }

  south_boundary_model <- if (nrow(south_data) >= 2) {
    lm(south_boundary ~ year_floor, data = south_data)
  } else {
    NULL
  }

  # Extract statistics safely
  n_trend_rate <- if (!is.null(north_boundary_model)) coef(north_boundary_model)[2] else NA
  n_p_val <- if (!is.null(north_boundary_model)) summary(north_boundary_model)$coefficients[2, 4] else NA
  n_r_squared <- if (!is.null(north_boundary_model)) summary(north_boundary_model)$r.squared else NA

  s_trend_rate <- if (!is.null(south_boundary_model)) coef(south_boundary_model)[2] else NA
  s_p_val <- if (!is.null(south_boundary_model)) summary(south_boundary_model)$coefficients[2, 4] else NA
  s_r_squared <- if (!is.null(south_boundary_model)) summary(south_boundary_model)$r.squared else NA


  # Additional quality check - flag extreme rate changes
  n_reasonable <- ifelse(!is.na(n_trend_rate),
    abs(n_trend_rate) <
      (max(species_extent_df$north_boundary, na.rm = TRUE) -
        min(species_extent_df$north_boundary, na.rm = TRUE)) / 5,
    NA
  )

  s_reasonable <- ifelse(!is.na(s_trend_rate),
    abs(s_trend_rate) <
      (max(species_extent_df$south_boundary, na.rm = TRUE) -
        min(species_extent_df$south_boundary, na.rm = TRUE)) / 5,
    NA
  )

  # FIXED: Return correct dataframe name
  return(list(
    species = species_name,
    # Original return values
    n_bound_pos_trend = ifelse(!is.na(n_trend_rate), n_trend_rate > 0, NA),
    n_trend_rate = n_trend_rate,
    n_p_val = n_p_val,
    n_r_squared = n_r_squared,
    s_bound_pos_trend = ifelse(!is.na(s_trend_rate), s_trend_rate > 0, NA),
    s_trend_rate = s_trend_rate,
    s_p_val = s_p_val,
    s_r_squared = s_r_squared,
    # Additional values for diagnostics and plotting
    n_reasonable = n_reasonable,
    s_reasonable = s_reasonable,
    boundaries_df = species_extent_df # FIXED: was species_df
  ))
}


range_plot <- function(species_name, range_results = NULL, title_prefix = "Coastline Range of") {
  if (is.null(range_results)) {
    range_results <- range_trend(species_name)
  }

  boundaries_df <- range_results$boundaries_df

  common_name <- species_names %>%
    filter(species_lump == range_results$species) %>%
    pull(common_name)

  if (is.null(boundaries_df) || nrow(boundaries_df) < 2) {
    warning(paste("Insufficient data to plot range for:", common_name))
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste("Insufficient data for", common_name)) +
        theme_void()
    )
  }

  boundaries_df_km <- boundaries_df %>%
    mutate(
      north_boundary_km = north_boundary / 1000,
      south_boundary_km = south_boundary / 1000
    )

  # Create a full sequence of years and get corresponding year_bin labels
  all_years <- seq(min(boundaries_df_km$year_floor, na.rm = TRUE),
    max(boundaries_df_km$year_floor, na.rm = TRUE),
    by = 5
  )

  # Get unique year_bin for each year_floor
  year_labels_df <- boundaries_df %>%
    select(year_floor, year_bin) %>%
    distinct() %>%
    filter(year_floor %in% all_years)

  # Rebuild with consistent x-axis values
  boundaries_df_km <- tibble(year_floor = all_years) %>%
    left_join(boundaries_df_km, by = "year_floor") %>%
    left_join(year_labels_df, by = "year_floor")


  # Calculate padded y-axis limits safely
  y_min_km <- min(boundaries_df_km$south_boundary_km, na.rm = TRUE)
  y_max_km <- max(boundaries_df_km$north_boundary_km, na.rm = TRUE)
  y_range_km <- y_max_km - y_min_km
  y_padding_km <- y_range_km * 0.1

  # Filter rows where both boundaries are non-NA for segment plotting
  segment_df <- boundaries_df_km %>%
    filter(!is.na(north_boundary_km) & !is.na(south_boundary_km))

  # Build the plot
  p <- ggplot(boundaries_df_km, aes(x = year_floor)) +
    geom_segment(
      data = segment_df,
      aes(xend = year_floor, y = north_boundary_km, yend = south_boundary_km),
      color = "grey50", size = 5, alpha = 0.7
    ) +
    geom_point(
      aes(y = north_boundary_km), color = "#349546", size = 10, na.rm = TRUE
      ) +
    geom_point(
      aes(y = south_boundary_km), color = "#00291f", size = 10, na.rm = TRUE
      ) +
    labs(
      title = paste(title_prefix, common_name),
      y = "Coastline Position (km)"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      axis.title = element_blank(),
      axis.text.x = element_text(
        angle = 45, vjust = .5
        ),
      # axis.title.y = element_text(
      #    margin = margin(r = 20), face = "bold", size = 24
      #   ),
      axis.text = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.margin = margin(t = 10, r = 40, b = 10, l = 10),
    ) +
    scale_y_continuous(
      limits = c(y_min_km - y_padding_km, y_max_km + y_padding_km),
      label = scales::label_number(suffix = "km")
    ) +
    scale_x_continuous(
      breaks = year_labels_df$year_floor,
      labels = year_labels_df$year_bin,
      limits = range(all_years)
    )


  # Add warning subtitle if needed
  if (!isTRUE(range_results$n_reasonable) || !isTRUE(range_results$s_reasonable)) {
    p <- p +
      labs(subtitle = paste(
        "WARNING: Potentially unrealistic rate of change detected -",
        "95th (red) and 5th (blue) percentiles of species distribution"
      ))
  }

  return(p)
}


gam_plot <- function(species_name, biodiv_df = clean_biodiv()) {
  # Filter for California and target species
  species_biodiv <- biodiv_df %>%
    left_join(
      marine_sites %>%
        select(marine_site_name, coastline_m),
      by = join_by(marine_site_name)
    ) %>%
    filter(
      state_province == "California",
      species_lump == species_name
    )

  species_pred <- gam_predict(species_name, biodiv_df)
  species_cum_den <- cum_den_df(species_biodiv)

  gam_plot <- ggplot(species_pred, aes(
    x = coastline_m / 1000,
    y = cum_den_norm,
    color = year_bin
  )) +
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper,
        fill = year_bin
      ),
      alpha = 0.2, color = NA
    ) +
    # geom_line(linewidth = 1) +
    geom_point(
      data = species_cum_den %>%
        filter(!is.na(cum_den_norm)),
      aes(y = cum_den_norm), alpha = 0.6, size = 1.2
    ) +
    # annotate this stating point conception
    geom_vline(xintercept = 520859.2599 / 1000, linetype = "dotted", color = "blue") +
    labs(
      title = paste(species_name, "- Cumulative Density"),
      x = "Distance along coastline\nfrom southern CA Border(km)",
      y = "Normalized Cumulative Density",
      color = "Year Bin",
      fill = "Year Bin"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    ) +
    facet_wrap(~year_bin)
  return(gam_plot)
}
