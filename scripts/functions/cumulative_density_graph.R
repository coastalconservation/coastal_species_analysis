cumulative_den_graph <- function(species_name) {
  
  # Filter cumulative density to species of interest
  species_cum_den <- cum_den_df(biodiv_df) %>% 
    filter(species_lump == species_name)

  # Fit cumulative densities to logistic distribution
  species_norm_logit <- glm(
    cum_den_norm ~ coastline_m * year_bin, 
    family = quasibinomial(link = "logit"), 
    data = species_cum_den
  )

  # Generate prediction grid
  species_pred <- expand_grid(
    coastline_m = seq(0, 1800000, length.out = 1000),
    year_bin = unique(species_cum_den$year_bin)
  )

  # Predict with confidence intervals (logit scale)
  pred <- predict(species_norm_logit, newdata = species_pred, type = "link", se.fit = TRUE)

  species_pred <- species_pred %>%
    mutate(
      fit_link = pred$fit,
      se_link = pred$se.fit,
      cum_den_norm = plogis(fit_link),
      lower = plogis(fit_link - 1.96 * se_link),
      upper = plogis(fit_link + 1.96 * se_link)
    )

  # Create plot with confidence ribbons
  extent_plot <- ggplot(species_pred, aes(y = coastline_m / 1000, 
                                          x = cum_den_norm,
                                          color = year_bin)) +
    geom_ribbon(aes(xmin = lower,
                    xmax = upper,
                    fill = year_bin),
                alpha = 0.2, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(data = species_cum_den %>%
                        filter(!is.na(cum_den_norm)),
               aes(x = cum_den_norm), alpha = 0.6, size = 1.2) +
    geom_vline(xintercept = 0.95, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 520859.2599 / 1000, linetype = "dotted", color = "blue") +
    scale_color_viridis_d(option = "B") +
    scale_fill_viridis_d(option = "B") +
    labs(
      title = paste(species_name, "- Cumulative Density"),
      y = "Coastline Distance (km)",
      x = "Normalized Cumulative Density",
      color = "Year Bin",
      fill = "Year Bin"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    ) #+
    #coord_fixed(ratio=1500)

  return(extent_plot)
}
