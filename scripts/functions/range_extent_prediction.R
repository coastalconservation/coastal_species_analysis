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
range_extent_df <- function(species_name) {
  # Get the cumulative density data for the specified species
  species_cum_den <- cum_den_df(biodiv_df) %>%
    filter(species_lump == species_name)

  # Fit logistic regression model to predict cumulative density from latitude and year
  species_norm_logit <- glm(
    cum_den_norm ~ coastline_m * year,
    family = quasibinomial(link = "logit"),
    data = species_cum_den
  )

  # Generate predictions across a grid of latitudes and years
  species_pred <- expand_grid(
    coastline_m = seq(0, 1800000, length.out = 1000),
    year = unique(species_cum_den$year)
  ) %>%
    mutate(
      cum_den_norm = predict(
        species_norm_logit,
        newdata = .,
        type = "response"
      )
    )

  # Calculate the 5th and 95th percentile latitudes for each year
  species_extent_df <- species_pred %>%
    group_by(year) %>%
    summarise(
      # Northern edge (95th percentile)
      max_lat = approx(cum_den_norm, coastline_m, xout = 0.90)$y,
      # Southern edge (5th percentile)
      min_lat = approx(cum_den_norm, coastline_m, xout = 0.10)$y
    ) %>%
    mutate(
      species_name = species_name
    )

  return(species_extent_df)
}

range_extent_plot <- function(species_extent_df) {
  ggplot(species_extent_df, aes(x = year)) +
    # Northern boundary (e.g., 95th percentile)
    geom_point(aes(y = max_lat), color = "#D73027", size = 2, alpha = 0.8) +
    # Southern boundary (e.g., 5th percentile)
    geom_point(aes(y = min_lat), color = "#4575B4", size = 2, alpha = 0.8) +
    # Point Conception latitude line (replace 520859.2599 with actual latitude if available)
    geom_hline(yintercept = 520859.2599, linetype = "dashed", color = "darkgray") +
    annotate("text",
      x = min(species_extent_df$year), y = 440820 + 0.5,
      label = "Point Conception", hjust = 0, color = "darkgray", size = 3.5
    ) +
    labs(
      title = paste("5th and 95th Percentile Range of", species_extent_df$species_name[1]),
      x = "Year",
      y = "Coastline Distance",
      caption = "Red: Northern boundary (95th percentile), Blue: Southern boundary (5th percentile)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.caption = element_text(size = 10, color = "gray30"),
      axis.title = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
}
