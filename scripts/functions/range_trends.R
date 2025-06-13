#' Predict Species Distribution Using GAM Models
#'
#' This function fits Generalized Additive Models (GAM) to predict species
#' distribution patterns along the California coastline for different times.
#' It creates smooth prediction curves showing cumulative density normalized
#' distributions with confidence intervals.
#'
#' @param species_name Character string specifying the target species name
#'   (should match values in species_lump column)
#' @param biodiv_df Data frame containing biodiversity data with columns:
#'   marine_site_name, state_province, species_lump, and other species data
#'
#' @return A tibble containing GAM predictions with columns:
#'   \item{year_bin}{Time period grouping}
#'   \item{coastline_m}{Distance along coastline in meters}
#'   \item{cum_den_norm}{Normalized cumulative density (0-1)}
#'   \item{fit}{GAM fitted values}
#'   \item{se}{Standard errors}
#'   \item{lower}{Lower confidence interval boundary}
#'   \item{upper}{Upper confidence interval boundary}
#' @export
gam_predict <- function(species_name, biodiv_df) {
  # Filter for California and target species
  species_biodiv <- biodiv_df %>%
  # Join with marine sites to get coastline distances
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
  # This transforms raw occurrence data into normalized cumulative distributions
  species_cum_den <- cum_den_df(species_biodiv)

  # Step 2: Split by year_bin for temporal analysis
  # Each year_bin represents a different time period for comparison
  species_cum_den_split <- species_cum_den %>%
    group_by(year_bin) %>%
    group_split()

  # Step 3: Fit GAM models per year_bin with comprehensive error handling
  species_pred <- map_dfr(species_cum_den_split, function(data_chunk) {
    year_bin_val <- unique(data_chunk$year_bin)

    # Try to fit GAM with smooth spline - handles potential fitting failures
    model <- tryCatch(
      gam(cum_den_norm ~ s(coastline_m), data = data_chunk),
      error = function(e) {
        warning(paste("GAM fitting failed for year bin:", year_bin_val))
        return(NULL)
      }
    )

    # Return placeholder data if GAM fitting fails (insufficient data, etc.)
    if (is.null(model)) {
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

    # Create prediction grid across full coastline range
    # Uses 200 points for smooth curves
    coastline_seq <- seq(
      min(data_chunk$coastline_m, na.rm = TRUE),
      max(data_chunk$coastline_m, na.rm = TRUE),
      length.out = 200
    )

    pred_df <- tibble(
      year_bin = year_bin_val,
      coastline_m = coastline_seq
    )

    # Generate GAM predictions with standard errors for confidence intervals
    pred_vals <- predict(model, newdata = pred_df, type = "link", se.fit = TRUE)
    pred_df <- pred_df %>%
      mutate(
        # Get fitted values and clamp between 0-1 for biological realism
        cum_den_norm = predict(model, newdata = pred_df),
        cum_den_norm = pmin(pmax(cum_den_norm, 0), 1), # Clamp between 0 and 1
        fit = pred_vals$fit,
        se = pred_vals$se.fit,
        # Calculate 95% confidence intervals, also clamped to 0-1
        lower = pmax(0, fit - 1.96 * se),
        upper = pmin(1, fit + 1.96 * se)
      )

    return(pred_df)
  })

  return(species_pred)
}

#' Plot GAM Model Predictions with Observed Data
#'
#' Creates a comprehensive visualization of GAM model predictions showing
#' species cumulative density distributions along the California coastline.
#' Displays fitted curves with confidence intervals and observed data points.
#'
#' @param species_name Character string specifying the target species name
#' @param biodiv_df Data frame containing biodiversity data.
#'
#' @return A ggplot2 object with faceted panels showing:
#'   - Colored ribbons: 95% confidence intervals for GAM predictions
#'   - Points: Observed cumulative density data
#'   - Vertical line: Point Conception reference location
#'   - Separate facets for each time period (year_bin)
#'
#' @export
gam_plot <- function(species_name, biodiv_df = clean_biodiv()) {
  # Filter for California and target species with coastline distance data
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

  # Get GAM predictions and observed cumulative density data
  species_pred <- gam_predict(species_name, biodiv_df)
  species_cum_den <- cum_den_df(species_biodiv)

  # Create comprehensive GAM visualization
  gam_plot <- ggplot(species_pred, aes(
    x = coastline_m / 1000,  # Convert to km for readability
    y = cum_den_norm,
    color = year_bin
  )) +
    # Confidence interval ribbons
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper,
        fill = year_bin
      ),
      alpha = 0.2, color = NA
    ) +
    # Observed data points overlaid on predictions
    geom_point(
      data = species_cum_den %>%
        filter(!is.na(cum_den_norm)),
      aes(y = cum_den_norm), alpha = 0.6, size = 1.2
    ) +
    # Point Conception reference line - important biogeographic boundary
    geom_vline(
      xintercept = 520859.2599 / 1000, linetype = "dotted", color = "blue"
      ) +
    labs(
      title = paste(species_name, "- Cumulative Density"),
      x = "Distance along coastline\nfrom southern CA Border(km)",
      y = "Normalized Cumulative Density",
      color = "Year Bin",
      fill = "Year Bin"
    ) +
    # Clean theme with legend at bottom
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    ) +
    # Separate panels for each time period
    facet_wrap(~year_bin)

  return(gam_plot)
}

#' Analyze Species Range Trends Over Time
#'
#' This function analyzes temporal trends in species range boundaries by
#' calculating north and south distribution limits and fitting linear models
#' to detect range shifts over time periods.
#'
#' @param species_name Character string specifying the target species name
#' @param biodiv_df Data frame containing biodiversity data.
#'
#' @return A list containing trend analysis results:
#'   \item{species}{Input species name}
#'   \item{n_bound_pos_trend}{Logical, TRUE if north boundary is moving north}
#'   \item{n_trend_rate}{Rate of northern boundary change (m/year)}
#'   \item{n_p_val}{P-value for northern boundary trend significance}
#'   \item{n_r_squared}{R-squared value for northern boundary model fit}
#'   \item{s_bound_pos_trend}{Logical, TRUE if south boundary is moving north}
#'   \item{s_trend_rate}{Rate of southern boundary change (m/year)}
#'   \item{s_p_val}{P-value for southern boundary trend significance}
#'   \item{s_r_squared}{R-squared value for southern boundary model fit}
#'   \item{n_reasonable}{Quality flag for north boundary trend reasonableness}
#'   \item{s_reasonable}{Quality flag for south boundary trend reasonableness}
#'   \item{boundaries_df}{Data frame with boundary positions by year}
#'
#' @export
range_trend <- function(species_name, biodiv_df = clean_biodiv()) {
  # Get GAM predictions for the species across time periods
  species_pred <- gam_predict(species_name, biodiv_df)

  # Safe interpolation function with robust error handling
  # Handles cases with insufficient data or duplicate x-values
  safe_approx <- function(x, y, xout) {
    # Remove NAs and ensure we have valid data points
    valid_idx <- !is.na(x) & !is.na(y)
    if (sum(valid_idx) < 2) {
      return(NA)
    }

    x <- x[valid_idx]
    y <- y[valid_idx]

    # Remove duplicate x-values which can cause interpolation issues
    dedup <- tibble(x = x, y = y) %>%
      arrange(x) %>%
      distinct(x, .keep_all = TRUE)

    if (nrow(dedup) < 2) {
      return(NA)
    }

    # Perform interpolation with rule=2 (extends to boundaries)
    approx(dedup$x, dedup$y, xout = xout, rule = 2)$y
  }

  # Calculate species range boundaries for each time period
  # North boundary = 95th percentile of distribution (higher coastline values)
  # South boundary = 5th percentile of distribution (lower coastline values)
  species_extent_df <- species_pred %>%
    group_by(year_bin) %>%
    summarise(
      # Extract year from year_bin string for trend analysis
      year_floor = as.integer(substr(first(year_bin), 1, 4)),
      # North boundary should be at higher coastline values (95th percentile)
      north_boundary = safe_approx(cum_den_norm, coastline_m, xout = 0.95),
      # South boundary should be at lower coastline values (5th percentile)
      south_boundary = safe_approx(cum_den_norm, coastline_m, xout = 0.05),
      .groups = "drop"
    ) %>%
    select(year_bin, north_boundary, south_boundary, year_floor)

  # Filter data for model fitting - only use years with valid boundary data
  north_data <- species_extent_df %>%
    filter(!is.na(north_boundary))

  south_data <- species_extent_df %>%
    filter(!is.na(south_boundary))

  # Fit linear models to detect temporal trends (need at least 2 data points)
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

  # Extract trend statistics safely - handle cases with failed model fitting
  n_trend_rate <- if (
    !is.null(north_boundary_model)
    ) coef(north_boundary_model)[2] else NA
  n_p_val <- if (
    !is.null(north_boundary_model)
    ) summary(north_boundary_model)$coefficients[2, 4] else NA
  n_r_squared <- if (
    !is.null(north_boundary_model)
    ) summary(north_boundary_model)$r.squared else NA

  s_trend_rate <- if (
    !is.null(south_boundary_model)
    ) coef(south_boundary_model)[2] else NA
  s_p_val <- if (
    !is.null(south_boundary_model)
    ) summary(south_boundary_model)$coefficients[2, 4] else NA
  s_r_squared <- if (
    !is.null(south_boundary_model)
    ) summary(south_boundary_model)$r.squared else NA

  # Quality check - flag extremely unrealistic rate changes
  # Prevents reporting of artifacts from poor model fits
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

  # Return comprehensive results list
  return(list(
    species = species_name,
    # Trend direction (positive = northward expansion)
    n_bound_pos_trend = ifelse(!is.na(n_trend_rate), n_trend_rate > 0, NA),
    n_trend_rate = n_trend_rate,
    n_p_val = n_p_val,
    n_r_squared = n_r_squared,
    s_bound_pos_trend = ifelse(!is.na(s_trend_rate), s_trend_rate > 0, NA),
    s_trend_rate = s_trend_rate,
    s_p_val = s_p_val,
    s_r_squared = s_r_squared,
    # Quality control flags and raw data for plotting
    n_reasonable = n_reasonable,
    s_reasonable = s_reasonable,
    boundaries_df = species_extent_df
  ))
}


#' Plot Species Range Boundaries Over Time
#'
#' Creates a visualization showing the temporal changes in species range
#' boundaries along the California coastline. Displays both northern and
#' southern range limits with connecting segments to show range extent.
#'
#' @param species_name Character string specifying the target species name
#' @param range_results Optional list output from range_trend(). If NULL,
#'   the function will calculate range trends internally
#' @param title_prefix Character string to prepend to the plot title.
#'   Default: "Coastline Range of"
#'
#' @return A ggplot2 object showing:
#'   - Green points: Northern range boundaries (95th percentile)
#'   - Dark green points: Southern range boundaries (5th percentile)
#'   - Gray segments: Range extent connecting north and south boundaries
#'   - Warning subtitle if unrealistic trends are detected
#'
#' @export
range_plot <- function(species_name, range_results = NULL, title_prefix = "Coastline Range of") {
  # Calculate range trends if not provided
  if (is.null(range_results)) {
    range_results <- range_trend(species_name)
  }

  # Extract boundary data from range analysis results
  boundaries_df <- range_results$boundaries_df

  # Get common name for prettier plot titles
  common_name <- species_names %>%
    filter(species_lump == range_results$species) %>%
    pull(common_name)

  # Handle cases with insufficient data for plotting
  if (is.null(boundaries_df) || nrow(boundaries_df) < 2) {
    warning(paste("Insufficient data to plot range for:", common_name))
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste("Insufficient data for", common_name)) +
        theme_void()
    )
  }

  # Convert distances from meters to kilometers for better readability
  boundaries_df_km <- boundaries_df %>%
    mutate(
      north_boundary_km = north_boundary / 1000,
      south_boundary_km = south_boundary / 1000
    )

  # Create consistent x-axis with all time periods
  all_years <- seq(min(boundaries_df_km$year_floor, na.rm = TRUE),
    max(boundaries_df_km$year_floor, na.rm = TRUE),
    by = 5
  )

  # Get unique year_bin labels for each year_floor
  year_labels_df <- boundaries_df %>%
    select(year_floor, year_bin) %>%
    distinct() %>%
    filter(year_floor %in% all_years)

  # Rebuild dataframe with consistent x-axis values, including missing years
  boundaries_df_km <- tibble(year_floor = all_years) %>%
    left_join(boundaries_df_km, by = "year_floor") %>%
    left_join(year_labels_df, by = "year_floor")

  # Calculate appropriate y-axis limits with padding
  y_min_km <- min(boundaries_df_km$south_boundary_km, na.rm = TRUE)
  y_max_km <- max(boundaries_df_km$north_boundary_km, na.rm = TRUE)
  y_range_km <- y_max_km - y_min_km
  y_padding_km <- y_range_km * 0.1

  # Filter rows for segment plotting - only where both boundaries exist
  segment_df <- boundaries_df_km %>%
    filter(!is.na(north_boundary_km) & !is.na(south_boundary_km))

  # Build the main plot with range visualization
  p <- ggplot(boundaries_df_km, aes(x = year_floor)) +
    # Gray segments showing range extent
    geom_segment(
      data = segment_df,
      aes(xend = year_floor, y = north_boundary_km, yend = south_boundary_km),
      color = "grey50", size = 5, alpha = 0.7
    ) +
    # Northern boundary points (lighter green)
    geom_point(
      aes(y = north_boundary_km), color = "#349546", size = 10, na.rm = TRUE
      ) +
    # Southern boundary points (darker green)
    geom_point(
      aes(y = south_boundary_km), color = "#00291f", size = 10, na.rm = TRUE
      ) +
    labs(
      title = paste(title_prefix, common_name),
      y = "Coastline Position (km)"
    ) +
    # Clean, professional theme
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      axis.title = element_blank(),
      axis.text.x = element_text(
        angle = 45, vjust = .5
        ),
      axis.text = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.margin = margin(t = 10, r = 40, b = 10, l = 10),
    ) +
    # Y-axis formatting with km labels
    scale_y_continuous(
      limits = c(y_min_km - y_padding_km, y_max_km + y_padding_km),
      label = scales::label_number(suffix = "km")
    ) +
    # X-axis with year_bin labels
    scale_x_continuous(
      breaks = year_labels_df$year_floor,
      labels = year_labels_df$year_bin,
      limits = range(all_years)
    )

  # Add warning subtitle if unrealistic trends detected
  if (
    !isTRUE(range_results$n_reasonable) || !isTRUE(range_results$s_reasonable)
    ) {
    p <- p +
      ggplot2::labs(subtitle = paste(
        "WARNING: Potentially unrealistic rate of change detected -",
        "95th (red) and 5th (blue) percentiles of species distribution"
      ))
  }

  return(p)
}