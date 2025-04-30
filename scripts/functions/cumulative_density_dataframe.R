#' Calculate Cumulative Density and Normalized Cumulative Density
#'
#' This function processes the cleaned MARINe surveys dataframe to compute the 
#' cumulative sum of density values (`cum_den`) and the normalized cumulative 
#' density (`cum_den_norm`) for each species group and year.
#' 
#' The function filters out entries where the `collection_source` is "point contact", 
#' focusing on those collected through "quadrat" and "swath" methods, 
#' groups the data by `species_lump` and `year`, arranges it by latitude within each group,
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
#'   \item{cum_den}{Cumulative sum of `density_per_m2` within each species-year group.}
#'   \item{cum_den_norm}{Normalized cumulative density (cumulative sum divided by the group's maximum cumulative sum).}
#'   \item{latitude}{Latitude corresponding to the measurement (to be replaced with distance along coast).}
#'   \item{year}{Survey year.}
#'   \item{species_lump}{Grouped species identifier.}
#' }
#'
#' @import dplyr
#' @export
cum_den_df <- function(bio_df){
  bio_df <- bio_df %>% 
    # Remove entries where the collection source is "point contact"
    filter(collection_source != "point contact") %>% 
    # Group data by species and year
    group_by(species_lump, year) %>% 
    # Arrange within each group by latitude
    arrange(latitude) %>% 
    # Calculate cumulative density and normalized cumulative density
    mutate(
      cum_den = cumsum(density_per_m2),
      cum_den_norm = cum_den / max(cum_den),
      # ecdf_function = ecdf(density_per_m2)
      ecdf_values = ecdf(density_per_m2)(density_per_m2)
    ) %>% 
    # Select only relevant columns for output
    select(cum_den, cum_den_norm, ecdf_values, latitude, year, species_lump)
  
  # Return the processed dataframe
  return(bio_df)
}

