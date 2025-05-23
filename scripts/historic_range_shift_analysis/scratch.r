  # Filter for California and target species
  species_biodiv <- biodiv_df %>%
    left_join(
      marine_sites %>%
        select(marine_site_name, coastline_m),
      by = join_by(marine_site_name)
    ) %>%
    filter(
      state_province == "California",
      species_lump == "Macron lividus"
    )

  # Step 1: Create cumulative density dataframe (using your existing function)
  species_cum_den <- cum_den_df(species_biodiv)

  # Check for sufficient data
  if (
    nrow(species_cum_den) < 10 ||
     all(species_cum_den$cum_den == 0, na.rm = TRUE)
     ) {
    warning(paste("Insufficient or all-zero data for:", species_name))
    return(list(
      n_bound_pos_trend = NA,
      s_bound_pos_trend = NA
    ))
  }

  # Get all unique year bins
  year_bins <- unique(species_cum_den$year_bin)

  # Create empty dataframe to store boundary results
  species_df <- tibble(
    year_bin = character(),
    north_boundary = numeric(),
    south_boundary = numeric(),
    year_floor = integer()
  )

  # Process each year bin separately
  for (bin in year_bins) {
    # Filter data for current year bin
    bin_data <- species_cum_den %>%
      filter(year_bin == bin)

    # Only proceed if we have enough data points in this bin
    if (nrow(bin_data) >= 5) {
      # Fit GAM model to observed data only (no artificial boundaries)
      bin_gam <- try(mgcv::gam(
        cum_den_norm ~ s(coastline_m,  k = 5),
        family = quasibinomial(link = "logit"),
        data = bin_data
      ), silent = TRUE)

      # Check if model fit successfully
      if (!inherits(bin_gam, "try-error")) {
        # Create prediction grid with buffer for extrapolation
        # Use 20% buffer beyond observed range for reasonable extrapolation
        min_coast <- min(bin_data$coastline_m, na.rm = TRUE)
        max_coast <- max(bin_data$coastline_m, na.rm = TRUE)
        range_coast <- max_coast - min_coast
        buffer <- range_coast * 0.2

        pred_grid <- tibble(
          coastline_m = seq(
            min_coast,
            max_coast,
            length.out = 1000
          )
        )

        # Generate predictions
        pred_grid$cum_den_norm <- predict(
            bin_gam,
            newdata = pred_grid,
            type = "response"
            )

        # Interpolate to find 5th and 95th percentiles
        # Use approx with rule=2 for extrapolation if needed
        north_boundary <- try(approx(
          pred_grid$cum_den_norm, pred_grid$coastline_m,
          xout = 0.95, rule = 2
        )$y, silent = TRUE)

        south_boundary <- try(approx(
          pred_grid$cum_den_norm, pred_grid$coastline_m,
          xout = 0.05, rule = 2
        )$y, silent = TRUE)

        # Check if interpolation was successful and results are reasonable
        if (!inherits(north_boundary, "try-error") &&
          !inherits(south_boundary, "try-error") &&
          !is.na(north_boundary) && !is.na(south_boundary)) {
          # Add to results dataframe
          year_floor <- as.integer(substr(bin, 1, 4))
          species_df <- species_df %>%
            add_row(
              year_bin = bin,
              north_boundary = north_boundary,
              south_boundary = south_boundary,
              year_floor = year_floor
            )
        }
      }
    }
  }

  # Check if we have enough data points for trend analysis
  if (nrow(species_df) < 3) {
    warning(paste("Insufficient year bins with valid boundaries for:", species_name))
    return(list(
      n_bound_pos_trend = NA,
      s_bound_pos_trend = NA
    ))
  }

  # Step 6: Fit linear models to track boundary movement over time
  north_boundary_model <- lm(north_boundary ~ year_floor, data = species_df)
  south_boundary_model <- lm(south_boundary ~ year_floor, data = species_df)

  # Calculate diagnostic stats
  n_trend_rate <- coef(north_boundary_model)[2] %>% unname()
  n_p_val <- summary(north_boundary_model)$coefficients[2, 4]
  n_r_squared <- summary(north_boundary_model)$r.squared
  s_trend_rate <- coef(south_boundary_model)[2] %>% unname()
  s_p_val <- summary(south_boundary_model)$coefficients[2, 4]
  s_r_squared <- summary(south_boundary_model)$r.squared

  # Additional quality check - flag extreme rate changes
  n_reasonable <- abs(n_trend_rate) < (max(species_df$north_boundary) - min(species_df$north_boundary)) / 2
  s_reasonable <- abs(s_trend_rate) < (max(species_df$south_boundary) - min(species_df$south_boundary)) / 2

  # Return analysis results along with the boundary dataframe for plotting
  return(list(
    species = species_name,
    # Original return values
    n_bound_pos_trend = n_trend_rate > 0,
    n_trend_rate = n_trend_rate,
    n_p_val = n_p_val,
    n_r_squared = n_r_squared,
    s_bound_pos_trend = s_trend_rate > 0,
    s_trend_rate = s_trend_rate,
    s_p_val = s_p_val,
    s_r_squared = s_r_squared,
    # Additional values for diagnostics and plotting
    n_reasonable = n_reasonable,
    s_reasonable = s_reasonable,
    boundaries_df = species_df
  ))


  # Extract the boundaries dataframe
  boundaries_df <- species_df

  # Check if we have valid data
  if (is.null(boundaries_df) || nrow(boundaries_df) < 2) {
    warning(paste("Insufficient data to plot range for:", species_name))
    # Return empty plot with message
    return(
      ggplot() +
        annotate(
          "text", x = 0.5, y = 0.5,
          label = paste("Insufficient data for", species_name)
        ) +
        theme_void()
    )
  }

  # Convert boundaries to km for plotting
  boundaries_df_km <- boundaries_df %>%
    mutate(
      north_boundary_km = north_boundary / 1000,
      south_boundary_km = south_boundary / 1000
    )

  # Calculate y-axis range with some padding in km
  y_min_km <- min(boundaries_df_km$south_boundary_km, na.rm = TRUE)
  y_max_km <- max(boundaries_df_km$north_boundary_km, na.rm = TRUE)
  y_range_km <- y_max_km - y_min_km
  y_padding_km <- y_range_km * 0.1 # 10% padding

  # Create the plot
  p <- ggplot(boundaries_df_km, aes(x = year_floor)) +
    # Add vertical range lines
    geom_segment(
      aes(
        x = year_floor, xend = year_floor,
        y = north_boundary_km, yend = south_boundary_km
      ),
      color = "grey50", size = 1, alpha = 0.7
    ) +
    # Add boundary points
    geom_point(aes(y = north_boundary_km), color = "firebrick3", size = 3) +
    geom_point(aes(y = south_boundary_km), color = "darkblue", size = 3) +
    # Improve labels and titles
    labs(
      title = "tite",#paste(title_prefix, species_name),
      subtitle = "95th (red) and 5th (blue) percentiles of species distribution",
      y = "Coastline Position (km)",
      x = "Year"
    ) +
    # Enhanced theme elements
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(face = "italic", size = 12),
      plot.caption = element_text(hjust = 0, face = "italic", size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      axis.title = element_text(face = "bold")
    ) +
    # Adjust y-axis with padding
    scale_y_continuous(limits = c(y_min_km - y_padding_km, y_max_km + y_padding_km)) +
    # Adjust x-axis to appropriate breaks
    scale_x_continuous(breaks = pretty_breaks(n = 5))

  # Add warning if results are flagged as unreasonable
  if (!is.null(range_results$n_reasonable) && !range_results$n_reasonable ||
    !is.null(range_results$s_reasonable) && !range_results$s_reasonable) {
    p <- p +
      labs(subtitle = paste(
        "WARNING: Potentially unrealistic rate of change detected -",
        "95th (red) and 5th (blue) percentiles of species distribution"
      ))
  }
