#' Create Cumulative Density Graph for Species Range Analysis
#'
#' @description
#' This function creates a visualization of the cumulative density distribution 
#' by latitude for a specific species across different years. It models the 
#' distribution using logistic regression and highlights the northern range edge 
#' (defined as where cumulative density reaches 95%).
#'
#' @param species_name Character string specifying the species name to analyze 
#'   (must match values in the species_lump column of the input data).
#'
#' @return A ggplot object displaying cumulative density curves by latitude
#'   across years with the northern range edge threshold highlighted.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Filters cumulative density data for the specified species
#'   \item Fits a logistic regression model predicting normalized cumulative 
#'         density from latitude and year
#'   \item Creates predictions across latitudes 32-36 degrees for years 2000-2024
#'   \item Calculates northern (95% density) and southern (5% density) range edges
#'   \item Visualizes the cumulative density curves with the 95% threshold marked
#' }
#'
#' @examples
#' \dontrun{
#' # Create cumulative density graph for a specific species
#' cumulative_den_graph("Roperia poulsoni")
#' 
#' # Save the plot to a file
#' my_plot <- cumulative_den_graph("Roperia poulsoni")
#' ggsave("roperia__range_shift.png", my_plot, width = 8, height = 6)
#' }
#'
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line xlim geom_hline labs
#' @importFrom tidyr expand_grid
#' @importFrom stats glm binomial predict approx
#'
#' @seealso \code{\link{cum_den_df}} for the function that prepares cumulative 
#'   density data
#'
#' @export
cumulative_den_graph <- function(species_name){
  
  # Filter cumulative density to species of interest
  species_cum_den <- cum_den_df(biodiv_df) %>% 
    filter(species_lump == species_name)
  
  # Fit cumulative densities to logistic distribution
  species_norm_logit <- glm(cum_den_norm ~ coastline_km * year_bin, 
                       binomial(link = "logit"), 
                       species_cum_den)
  
  # Calculate predicitons
  species_pred <- expand_grid(
    coastline_km = seq(64727.0844, 797950.6234, length.out = 1000),
     year_bin = species_cum_den$year_bin %>% unique()
                              ) %>% 
    mutate(
      cum_den_norm = predict(species_norm_logit,
                             newdata = ., type = "response"),
    )

  # Create plot of range extents
  extent_plot <- ggplot(species_cum_den, aes(x = coastline_km, 
                                             y = cum_den_norm, 
                                             color = year_bin)) +
    geom_point() +
    geom_line(aes(group = year_bin), data=species_pred) +
    xlim(64727.0844, 797950.6234) +
    geom_hline(yintercept = .95) +
    geom_vline(xintercept = 520859.2599) +
    labs(title = paste(species_name, " - Cumulative Density"))
  
  return(extent_plot)
}