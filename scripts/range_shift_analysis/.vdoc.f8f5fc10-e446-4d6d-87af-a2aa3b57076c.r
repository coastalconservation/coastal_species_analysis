#
#
#
#
#
#
#
#
#
#| code-fold: true
library(here)
library(readr)
library(lubridate)
library(tidyverse)
library(stats) # Load necessary libraries
library(here) # For building file paths relative to project root
library(readr) # For reading CSV files
library(lubridate) # For working with date/time data (used in sourced scripts)
library(dplyr) # Data manipulation
library(tidyr) # Data tidying
library(purrr) # Functional programming tools
library(ggplot2) # Plotting
library(mgcv) # Generalized Additive Models
library(scales)
#
#
#
#
# Source cleaning function
source(here("scripts", "functions", "clean_biodiv.R"))
source(here("scripts", "functions", "cumulative_density_dataframe.R"))
source(here("scripts", "functions", "range_trends.R"))
source(here("scripts", "functions", "cumulative_density_graph.R"))
# Load data
species_extent <- read_csv("/capstone/coastalconservation/data/processed/species_extent.csv")
marine_sites <- read_csv("/capstone/coastalconservation/data/processed/marine_site_segments.csv") # Load processed datasets
processed_data_path <- "/capstone/coastalconservation/data/processed"
species_names <- read_csv("/capstone/coastalconservation/data/processed/species_names.csv")

biodiv_df <- read_csv(
    file.path(
        processed_data_path,
        "clean_biodiv_2025.csv"
    ),
    show_col_types = FALSE
)
#
#
#
gam_plot("Fucus spp")
gam_predict("Fucus spp", biodiv_df)
#
#
#
dangermond_boundary_species <- species_extent %>%
    filter(str_detect(southern_extent_name, "Point Conception") |
        str_detect(northern_extent_name, "Point Conception"))
#
#
#
dangermond_boundary_species
#
#
#
dangermond_boundary_species_list <- dangermond_boundary_species %>% pull(species_lump)
dangermond_boundary_species_list
#
#
#
#
northern_boundary_species <- dangermond_boundary_species %>%
    filter(
        str_detect(northern_extent_name, "Point Conception")
    ) %>%
    pull(species_lump)
#
#
#
northern_boundary_species
#
#
#
#
southern_boundary_species <- dangermond_boundary_species %>%
    filter(
        str_detect(southern_extent_name, "Point Conception")
    ) %>%
    pull(species_lump)
#
#
#
southern_boundary_species
#
#
#
northern_trends_df <- map_dfr(
    northern_boundary_species,
    function(species_name) {
        result <- tryCatch(
            range_trend(species_name),
            error = function(e) {
                warning(paste("Failed to compute range_trend for", species_name, ":", e$message))
                return(NULL)
            }
        )

        if (is.null(result)) {
            return(tibble(
                species = species_name,
                north_trend_positive = NA,
                n_trend_rate = NA,
                r_squared = NA,
                p_val = NA
            ))
        }

        tibble(
            species = species_name,
            north_trend_positive = unname(result$n_bound_pos_trend),
            n_trend_rate = unname(result$n_trend_rate),
            r_squared = unname(result$n_r_squared),
            p_val = unname(result$n_p_val)
        )
    }
)

#
#
#
#
write_csv(
    northern_trends_df,
    file = "/capstone/coastalconservation/data/processed/ntrends_stats.csv"
)
#
#
#
northern_trends_df %>% View()
#
#
#
northern_trends_df %>%
    filter(
        n_trend_rate > 200,
    ) %>%
    pull(species)
#
#
#
#
#
#
#
#
#
southern_trends_df <- map_dfr(
    southern_boundary_species,
    function(species_name) {
        result <- tryCatch(
            range_trend(species_name),
            error = function(e) {
                warning(paste("Failed to compute range_trend for", species_name, ":", e$message))
                return(NULL)
            }
        )

        if (is.null(result)) {
            return(tibble(
                species = species_name,
                north_trend_positive = NA,
                n_trend_rate = NA,
                r_squared = NA,
                p_val = NA
            ))
        }

        tibble(
            species = species_name,
            north_trend_positive = unname(result$s_bound_pos_trend),
            n_trend_rate = unname(result$s_trend_rate),
            r_squared = unname(result$s_r_squared),
            p_val = unname(result$s_p_val)
        )
    }
)
#
#
#
write_csv(
    southern_trends_df,
    file = "/capstone/coastalconservation/data/processed/strends_stats.csv"
)
#
#
#
southern_trends_df %>% View()
#
#
#
southern_trends_df %>%
    filter(
        n_trend_rate > 200,
    ) %>%
    pull(species)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
dangermond_boundaries_df <- map_dfr(
    dangermond_boundary_species_list,
    function(species_name) {
        result <- tryCatch(
            range_trend(species_name),
            error = function(e) {
                warning(paste("Failed to compute range_trend for", species_name, ":", e$message))
                return(NULL)
            }
        )

        boundaries <- result$boundaries_df

        if (is.null(result)) {
            return(tibble(
                species = species_name,
                year_bin = boundaries$year_bin,
                year_floor = boundaries$year_floor,
                north_boundary = NA,
                south_boundary = NA,
                north_trend_positive = NA,
                south_trend_postive = NA

            ))
        }

        tibble(
            species = species_name,
            year_bin = boundaries$year_bin,
            year_floor = boundaries$year_floor,
            north_boundary = boundaries$north_boundary,
            south_boundary = boundaries$south_boundary,
            north_trend_positive = result$n_bound_pos_trend,
            south_trend_positive = result$s_bound_pos_trend
        )
    }
) %>% arrange(species)
#
#
#
north_boundaries_df <- dangermond_boundaries_df %>%
    filter(
        species %in% southern_boundary_species
    )

south_boundaries_df <- dangermond_boundaries_df %>%
    filter(
        species %in% northern_boundary_species
    )
#
#
#
saveRDS(
    north_boundaries_df,
    file = "/capstone/coastalconservation/data/processed/north_boundary_trends.rds"
)

saveRDS(
    south_boundaries_df,
    file = "/capstone/coastalconservation/data/processed/south_boundary_trends.rds"
)
#
#
#
