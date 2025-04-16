REDO_species_ranges <- function(cbs_clean, south_lat, north_lat) {
  csb_range_edge <- cbs_clean %>%
    group_by(species_lump) %>%
    summarize(
      present_below_south = any(latitude < south_lat & presence == TRUE),
      present_within_buffer = between(latitude, south_lat, north_lat) & presence == TRUE,
      present_above_north = any(latitude > north_lat & presence == TRUE),
      
      # Observations where Present
      site_observations = sum(present_within_buffer == TRUE & presence == TRUE, 
                                      na.rm = TRUE),
      
      # Distinct sites within buffer
      sites_in_buffer = n_distinct(marine_site_name[present_within_buffer ==  TRUE]),
      
      # Distinct sites where species are present within buffer
      sites_in_buffer_where_present = n_distinct(marine_site_name[present_within_buffer == TRUE & presence == 1]),
      
      # Total Counts
      total_counts = sum(num_count, na.rm = TRUE),
      
      # Fraction of years present
      percent_years_present = (n_distinct(year[between(latitude, south_lat, north_lat) 
                                              & presence == TRUE]) / 
        n_distinct(year)) * 100
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
