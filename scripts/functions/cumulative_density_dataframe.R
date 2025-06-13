#' Calculate Cumulative Density Distribution Along Coastline
#'
#' This function processes bio-survey data to calculate cumulative density
#' distributions of species along the California coastline. It groups data into
#' 5-year bins and computes normalized cumulative densities that can be used for
#' plotting species distribution curves.
#'
#' @param bio_df A data frame containing biological survey
#'  data with the following required columns:
#'   \itemize{
#'     \item \code{density_per_m2}: Numeric density measurements per m^2
#'     \item \code{percent_cover}: Numeric percent cover values
#'     \item \code{state_province}: Character vector of state/province names
#'     \item \code{year}: Numeric year of observation
#'     \item \code{species_lump}: Character vector of species names/groups
#'     \item \code{marine_site_name}: Character vector of site names
#'     \item \code{coastline_m}: Numeric distance along coastline in meters
#'   }
#'
#' @return A tibble with the following columns:
#'   \itemize{
#'     \item \code{species_lump}: Species name/group
#'     \item \code{year_bin}: 5-year time periods (e.g., "2000-2004")
#'     \item \code{coastline_m}: Distance along coastline in meters
#'     \item \code{cum_den}: Cumulative density (raw values)
#'     \item \code{cum_den_norm}: Normalized cumulative density (0-1 scale)
#'   }
#'
#' @export
cum_den_df <- function(bio_df) {
  # Determine which density metric to use based on data availability
  if (all(is.na(bio_df$density_per_m2))) {
    # Use percent cover if density data is entirely missing
    bio_df <- bio_df %>%
      dplyr::mutate(cum_norm = percent_cover)
  } else {
    # Use density per m2 as primary metric
    bio_df <- bio_df %>%
      dplyr::mutate(cum_norm = density_per_m2)
  }

  # Filter to California data and create 5-year time bins
  bio_df <- bio_df %>%
    filter(
      state_province == "California"
    ) %>%
    dplyr::mutate(
      # Create 5-year bins (e.g., 2000-2004, 2005-2009)
      year_bin = paste0(
        floor(year / 5) * 5, "-",
        floor(year / 5) * 5 + 4
      ),
      year_bin = as.factor(year_bin)
    ) %>%
    # Calculate mean density by species, site, coastline position, and time bin
    group_by(species_lump, marine_site_name, coastline_m, year_bin) %>%
    dplyr::summarise(
      mean_density = mean(cum_norm, na.rm = TRUE),
      .groups = "drop"
    )

  # Create a complete grid to ensure all species
  # x year_bin x coastline combinations exist
  full_grid <- expand.grid(
    species_lump = unique(bio_df$species_lump),
    year_bin = unique(bio_df$year_bin),
    coastline_m = sort(unique(bio_df$coastline_m))
  ) %>%
    dplyr::as_tibble()

  # Join observed data with complete grid and compute cumulative densities
  bio_df_full <- full_grid %>%
    dplyr::left_join(
      bio_df,
      by = c("species_lump", "year_bin", "coastline_m")
    ) %>%
    # Replace missing values with zero density
    dplyr::mutate(mean_density = replace_na(mean_density, 0)) %>%
    dplyr::arrange(species_lump, year_bin, coastline_m) %>%
    # Calculate cumulative density along coastline
    # for each species-year combination
    dplyr::group_by(species_lump, year_bin) %>%
    mutate(
      cum_den = cumsum(mean_density),
      cum_den_norm = cum_den / max(cum_den, na.rm = TRUE)
    ) %>%
    # Add anchor points for smooth plotting curves
    dplyr::group_modify(~ {
      # Add starting point at coastline position 0
      .x <- add_row(.x,
        cum_den = 0, cum_den_norm = 0, coastline_m = 0, .before = 1
      )
      # Add ending point at maximum coastline position
      .x <- add_row(.x,
        cum_den = 1, cum_den_norm = 1,
        coastline_m = max(.x$coastline_m, na.rm = TRUE),
        .after = nrow(.x)
      )
      .x
    }) %>%
    dplyr::ungroup() %>%
    # Select final columns for output
    dplyr::select(
      species_lump, year_bin, coastline_m, cum_den, cum_den_norm
    )

  return(bio_df_full)
}