#' Create range edge descriptions for each species at each buffer
#'
#' @description
#' This is a function that takes the dataframe given to it and creates a range edge description and summary statistics for each species at each buffer.
#' 
#' @param cbs_clean The biodiversity dataframe, cleaned by MarineBioClean.R
#' @param south_lat The absolute southern most latitude of the buffer
#' @param north_late The absolute northern most latitude of the buffer
#'
#' @returns Single dataframe at that buffer with range edge descriptions and summary statistics for each species
#' @export
#'
#' @examples
species_range <- function(cbs_clean, south_lat, north_lat) {
  csb_range_edge <- cbs_clean %>%
    group_by(species_lump) %>%
    summarize(
      present_below_south = any(latitude < south_lat & presence == 1),
      present_within_buffer = any(latitude >= south_lat & latitude <= north_lat & presence == 1),
      present_above_north = any(latitude > north_lat & presence == 1),
      
      # Observations where Present
      present_observations_site = sum(latitude >= south_lat & latitude <= north_lat & presence == 1, 
                                      na.rm = TRUE),
      
      # Observations Present/Absence
      total_observations = n(),
      
      # Distinct sites within buffer
      sites_in_buffer = n_distinct(marine_site_name[latitude >= south_lat & latitude <= north_lat]),
      
      # Distinct sites where species are present within buffer
      sites_present = n_distinct(marine_site_name[latitude >= south_lat & latitude <= north_lat & presence == 1]),
      
      # Total sites
      total_sites = n_distinct(marine_site_name),
      
      # Total Counts
      total_counts = sum(num_count, na.rm = TRUE),
      
      # Fraction of years present
      percent_years_present = n_distinct(year[latitude >= south_lat & latitude <= north_lat & presence == 1]) / 
        n_distinct(year) * 100
    ) %>%
    mutate(
      # Categorize species by range edges
      range_edge_category = case_when(
        present_above_north & present_within_buffer & !present_below_south ~ "Southern Range Edge",
        !present_above_north & present_within_buffer & present_below_south ~ "Northern Range Edge",
        present_above_north & present_within_buffer & present_below_south ~ "Continuous Presence",
        present_above_north & !present_within_buffer & present_below_south ~ "Avoidant",
        !present_above_north & !present_within_buffer & present_below_south ~ "Southern Only Presence",
        present_above_north & !present_within_buffer & !present_below_south ~ "Northern Only Presence",
        !present_above_north & present_within_buffer & !present_below_south ~ "Endemic Presence",
        TRUE ~ "Absent"
      )
    )
  
  return(csb_range_edge)
}