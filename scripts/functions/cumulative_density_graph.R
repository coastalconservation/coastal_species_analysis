cumulative_den_graph <- function(species_name) {
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
  # Filter cumulative density to species of interest
  species_cum_den <- cum_den_df(species_biodiv)
  species_pred <- tibble()
  year_bins <- unique(species_cum_den$year_bin)
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
        cum_den_norm ~ s(coastline_m, k = 5),
        family = quasibinomial(link = "logit"),
        data = bin_data
      ), silent = TRUE)

      # Check if model fit successfully
      if (!inherits(bin_gam, "try-error")) {
        min_coast <- min(bin_data$coastline_m, na.rm = TRUE)
        max_coast <- max(bin_data$coastline_m, na.rm = TRUE)
        range_coast <- max_coast - min_coast

        pred_grid <- tibble(
          coastline_m = seq(
            min_coast,
            max_coast,
            length.out = 1000
          )
        )

        # Generate predictions
        preds <- predict(bin_gam, newdata = pred_grid, type = "link", se.fit = TRUE)

        pred_grid <- pred_grid %>%
          mutate(
            cum_den_norm = binomial()$linkinv(preds$fit),
            lower = binomial()$linkinv(preds$fit - 1.96 * preds$se.fit),
            upper = binomial()$linkinv(preds$fit + 1.96 * preds$se.fit),
            year_bin = bin
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

  species_pred <- bind_rows(species_pred, pred_grid)

  # Create plot with confidence ribbons
  extent_plot <- ggplot(species_pred, aes(
    y = coastline_m / 1000,
    x = cum_den_norm,
    color = year_bin
  )) +
    geom_ribbon(
      aes(
        xmin = lower,
        xmax = upper,
        fill = year_bin
      ),
      alpha = 0.2, color = NA
    ) +
    geom_line(linewidth = 1) +
    geom_point(
      data = species_cum_den %>%
        filter(!is.na(cum_den_norm)),
      aes(x = cum_den_norm), alpha = 0.6, size = 1.2
    ) +
    # annotate this stating point conception
    geom_hline(yintercept = 520859.2599 / 1000, linetype = "dotted", color = "blue") +
    scale_color_viridis_d(option = "B") +
    scale_fill_viridis_d(option = "B") +
    labs(
      title = paste(species_name, "- Cumulative Density"),
      y = "Distance along coastline\nfrom southern CA Border(km)",
      x = "Normalized Cumulative Density",
      color = "Year Bin",
      fill = "Year Bin"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    ) #+
  # coord_fixed(ratio=1500)

  return(species_pred)
}
