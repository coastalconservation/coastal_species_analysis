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
range_extent_graph <- function(species_name){
  
  # Get the cumulative density data for the specified species
  species_cum_den <- cum_den_df(biodiv_df) %>% 
    filter(species_lump == species_name)
  
  # Fit logistic regression model to predict cumulative density from latitude and year
  species_logit <- glm(cum_den_norm ~ coastline_km * year, binomial(link="logit"), species_cum_den)
  
  # Generate predictions across a grid of latitudes and years
  species_pred <- expand_grid( # find appropriate distance
                              latitude = seq(32, 36, length.out=1000),
                              # distance = seq(32, 36, length.out=1000),
                              year = 2000:2024) %>% 
    mutate(
      cum_den_norm = predict(species_logit, newdata = ., type="response")
    ) 
  
  # Calculate the 5th and 95th percentile latitudes for each year
  species_extent_df <- species_pred %>% 
    group_by(year) %>% 
    summarise(
      # Northern edge (95th percentile)
      max_lat = approx(cum_den_norm, coastline_km, xout=0.95)$y, 
      # Southern edge (5th percentile)
      min_lat = approx(cum_den_norm, coastline_km, xout=0.05)$y  
    )
  
  # Create plot showing the range boundaries over time
  extent_plot <- ggplot(species_extent_df) +
    # Northern boundary in red
    geom_point(aes(year, max_lat), color="red") +     
    # Southern boundary in blue
    geom_point(aes(year, min_lat), color="blue") +    
    # Point Conception latitude line
    geom_hline(yintercept = 34.449) +                 
    labs(title= paste("5th and 95th Percentile Range: ", species_name))
  
  return(extent_plot)
}