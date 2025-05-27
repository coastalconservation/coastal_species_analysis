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
cum_den_df <- function(bio_df) {
  library(dplyr)
  library(tidyr)

  # Filter relevant data and create year bins
  bio_df <- bio_df %>%
    filter(
      state_province == "California",
      collection_source != "point contact"
    ) %>%
    mutate(
      year_bin = paste0(
        floor(year / 5) * 5, "-",
        floor(year / 5) * 5 + 4
      ),
      year_bin = as.factor(year_bin)
    ) %>%
    group_by(species_lump, marine_site_name, coastline_m, year_bin) %>%
    summarise(
      mean_density = mean(density_per_m2, na.rm = TRUE),
      .groups = "drop"
    )

  # Create a complete grid of species x year_bin x coastline_m
  full_grid <- expand.grid(
    species_lump = unique(bio_df$species_lump),
    year_bin = unique(bio_df$year_bin),
    coastline_m = sort(unique(bio_df$coastline_m))
  ) %>%
    as_tibble()

  # Join with observed data and compute cumulative and normalized densities
  bio_df_full <- full_grid %>%
    left_join(bio_df, by = c("species_lump", "year_bin", "coastline_m")) %>%
    mutate(mean_density = replace_na(mean_density, 0)) %>%
    arrange(species_lump, year_bin, coastline_m) %>%
    group_by(species_lump, year_bin) %>%
    mutate(
      cum_den = cumsum(mean_density),
      cum_den_norm = cum_den / max(cum_den, na.rm = TRUE)
    ) %>%
    # Add a row at coastline_m = 0 for plotting (to anchor curves at 0)
  group_modify(~ {
    .x <- add_row(.x,
      cum_den = 0, cum_den_norm = 0, coastline_m = 0, .before = 1)
    .x <- add_row(.x,
      cum_den = 1, cum_den_norm = 1,
      coastline_m = max(.x$coastline_m, na.rm = TRUE),
      .after = nrow(.x))
    .x
  }) %>%
    ungroup() %>%
    select(
      species_lump, year_bin, coastline_m, cum_den, cum_den_norm
    )

  return(bio_df_full)
}


