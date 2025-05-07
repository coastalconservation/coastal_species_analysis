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
  species_norm_logit <- glm(cum_den_norm ~ distance * year_bin, 
                       binomial(link="logit"), 
                       species_cum_den)
  
  species_ecdf_logit <- glm(cum_den_norm ~ distance * year_bin, 
                       binomial(link="logit"), 
                       species_cum_den)
  
  # Calculate predicitons
  species_pred <- expand_grid(distance = seq(32, 36, length.out=1000),
                              year_bin = species_cum_den$year_bin %>% unique()
                              ) %>% 
    mutate(
      cum_den_norm = predict(species_norm_logit,
                             newdata = ., type="response"),
      cum_den_ecdf = predict(species_ecdf_logit,
                             newdata = ., type="response")
    ) 
  
  # Approximate northern range (95th percentile) and southern range (5th percentile) by year
  species_extent_df <- species_pred %>% 
    group_by(year_bin) %>% 
    summarise(
      max_dist_norm = approx(cum_den_norm, distance, xout=0.95)$y,
      min_dist_norm = approx(cum_den_norm, distance, xout=0.05)$y,
      max_dist_ecdf = approx(cum_den_ecdf, distance, xout=0.95)$y,
      min_dist_ecdf = approx(cum_den_ecdf, distance, xout=0.05)$y
    )
  
  # Create plot of range extents
  extent_plot <- ggplot(species_cum_den, aes(x=distance, 
                                             y=cum_den_norm, 
                                             color=year_bin)) +
    geom_point() +
    geom_line(aes(group = year_bin), data=species_pred) +
    xlim(32, 36) +
    geom_hline(yintercept=.95) +
    geom_vline(xintercept = 34.449) +
    labs(title = paste(species_name, " - Cumulative Density"))
  
  return(extent_plot)
}