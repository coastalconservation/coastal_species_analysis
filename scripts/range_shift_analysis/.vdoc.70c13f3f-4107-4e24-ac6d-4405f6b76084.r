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
source(here("scripts", "functions", "historic_range_trends.R"))
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
#
gam_plot("Fucus spp")
gam_predict("Fucus spp", biodiv_df)
#
#
#
species_name <- "Fucus spp"

species_biodiv <- biodiv_df %>%
    left_join(
        marine_sites %>%
            select(marine_site_name, coastline_m),
        by = join_by(marine_site_name)
    ) %>%
    filter(
        state_province == "California",
        species_lump == species_name
    )


bio_df <- species_biodiv
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
#
#
#
dangermond_boundary_species <- species_extent %>%
    filter(str_detect(southern_extent_name, "Point Conception") |
        str_detect(northern_extent_name, "Point Conception") |
        str_detect(northern_extent_name, "Point Mugu"))
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
        # north_trend_positive == TRUE,
        n_trend_rate > 1000,
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
        # north_trend_positive == TRUE,
        n_trend_rate > 1000,
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
all_trends_df <- map_dfr(
    biodiv_df %>% pull(species_lump) %>% unique(),
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
            n_r_squared = unname(result$n_r_squared),
            n_p_val = unname(result$n_p_val),
            south_trend_positive = unname(result$s_bound_pos_trend),
            s_trend_rate = unname(result$s_trend_rate),
            s_r_squared = unname(result$s_r_squared),
            s_p_val = unname(result$s_p_val)
        )
    }
)
#
#
#
all_boundaries_df <- map_dfr(
    biodiv_df %>% pull(species_lump) %>% unique(),
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
                south_boundary = NA
            ))
        }

        tibble(
            species = species_name,
            year_bin = boundaries$year_bin,
            year_floor = boundaries$year_floor,
            north_boundary = boundaries$north_boundary,
            south_boundary = boundaries$south_boundary
        )
    }
) %>% arrange(species)
#
#
#
all_boundaries_df <- all_boundaries_df %>% arrange(species)
saveRDS(
    all_boundaries_df,
    file = "/capstone/coastalconservation/data/processed/boundaries.rds"
)
#
#
#
target_species <- c(
    "Agathistoma eiseni",
    "Aplysia californica",
    "Haminoea vesicula",
    "Norrisia norrisii",
    "Paraxanthias taylori",
    "Roperia poulsoni",
    "Tegula aureotincta",
    "Calliostoma annulatum", 
    "Calliostoma canaliculatum",
    "Diodora aspera",
    "Henricia spp",
    "Onchidella carpenteri",
    "Tegula brunnea"
)
#
#
#
target_boundaries <- readRDS(
    "/capstone/coastalconservation/data/processed/boundaries.rds"
) %>%
    filter(species %in% target_species)

saveRDS(
    target_boundaries,
    file = "/capstone/coastalconservation/data/processed/target_boundaries.rds"
)
#
#
#
