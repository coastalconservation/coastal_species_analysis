#' Calculate Cumulative Density and Normalized Cumulative Density in 5-Year Bins
#'
#' This function processes the cleaned MARINe surveys dataframe to compute the 
#' cumulative sum of density values (`cum_den`) and the normalized cumulative 
#' density (`cum_den_norm`) for each species group within 5-year bins.
#' 
#' The function filters out entries where the `collection_source` is "point contact", 
#' groups the data by `species_lump` and 5-year bin, arranges it by latitude within each group,
#' and calculates cumulative density metrics.
#'
#' @param bio_df A data frame containing at least the following columns:
#' \describe{
#'   \item{species_lump}{Species group identity}
#'   \item{year}{Survey year}
#'   \item{latitude}{Latitude of the survey point (to be replaced with distance along coast).}
#'   \item{density_per_m2}{Density measurement per square meter.}
#'   \item{collection_source}{Method of collection (e.g., "point contact").}
#' }
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{cum_den}{Cumulative sum of `density_per_m2` within each species-bin group.}
#'   \item{cum_den_norm}{Normalized cumulative density (cumulative sum divided by the group's maximum cumulative sum).}
#'   \item{latitude}{Latitude corresponding to the measurement (to be replaced with distance along coast).}
#'   \item{year_bin}{5-year bin of the survey year.}
#'   \item{species_lump}{Grouped species identifier.}
#' }
#'
#' @import dplyr
#' @export
cum_den_df <- function(bio_df){
  bio_df <- bio_df %>% 
    # Filter out point contact entries
    filter(collection_source != "point contact") %>%
    # Create a 5-year bin variable
    mutate(year_bin = paste0(floor(year / 5) * 5, "-", floor(year / 5) * 5 + 4)) %>%
    # Group by species and 5-year bin
    group_by(species_lump, year_bin) %>%
    # Arrange by coastline distance
    arrange(distance, .by_group = TRUE) %>%
    # Calculate cumulative and normalized density
    mutate(
      cum_den = cumsum(density_per_m2),
      cum_den_norm = cum_den / max(cum_den, na.rm = TRUE),
      ecdf_values = if (all(is.na(cum_den_norm)) || length(na.omit(cum_den_norm)) == 0) {
        NA_real_
      } else {
        ecdf(cum_den_norm)(cum_den_norm)
      }
    ) %>%
    # Select relevant columns
    select(cum_den, cum_den_norm, ecdf_values, distance
           latitude, state_province, year, year_bin, species_lump)
  
  return(bio_df)
  
  
}