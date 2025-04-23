# Downloading Bio-ORACLE environmental rasters
# Author: Jordan Sibley 
# File created: April 23, 2025 

# -----------------------------------------------------------------------------

# This document go through the process of downloading the environmental variables
# from Bio-ORACLE 

# Historical data time period: 2000-2010 (only goes in 10 year increments)
# Future projected data: 


# Environmental rasters 
# air temperature = tas 
# mixed layer depth = mlotst 
# ocean temperature = thetao 
# salinity = so 
# cloud fraction = clt 
# oxygen = o2


# -----------------------------------------------------------------------------

# load libraries 
library(biooracler)
library(here)
library(purrr)
library(terra)
library(testthat)



# Historical data -------------------------------------------------------------

### NOTES: looked online and it seems that the range of these files are only from 2000 to 2010


# Define layers of interest
layer_list_hist <- c("tas_baseline_2000_2020_depthsurf",
                     "o2_baseline_2000_2018_depthmax",
                     "mlotst_baseline_2000_2019_depthsurf",
                     "thetao_baseline_2000_2019_depthsurf",
                     "so_baseline_2000_2019_depthsurf",
                     "clt_baseline_2000_2020_depthsurf")

# Define constraints
constraints_hist <- list(time = c('2000-01-01T00:00:00Z', '2010-01-01T00:00:00Z'),
                    latitude = c(32, 43),
                    longitude = c(-126, -116))
names(constraints_hist) = c("time", "latitude", "longitude")

# Extract files 
env_00_list <- map(layer_list_hist, ~{
  layers <- download_layers(dataset_id = .x, constraints = constraints_hist, fmt = "raster")
  subset(layers, grep('_mean', names(layers), value = TRUE))  # Keep only mean layers
})

# Test that dimensions of rasters layers match before continuing 
test_that("All raster layers have matching dimensions", {
  dims <- map(env_00_list, dim) # get dimensions of each raster 
  first_dim <- dims[[1]] # get the dimensions of just the first layer
  
  all_match <- all(map_lgl(dims, ~ identical(.x, first_dim))) # test that each layer has same dimensions 
  
  expect_true(all_match,
              info = paste0("Not all raster dimensions match, no not move forward with combining into raster stack")
  )
})


# figure out how to subset to coastal buffer shapefile 
# i predict i can do this by doing a for loop 
# once it has been masked, then I can write the rasters out to raw data folder



# Combine into one multi-layer SpatRaster 
#env_00 <- do.call(c, env_00_list)

# Projected data -------------------------------------------------------------

# Define layers for future projected data under SSP 4.6 
layer_list_proj <- c("tas_ssp460_2020_2100_depthsurf",
                   "o2_ssp460_2020_2100_depthmax",
                   "mlotst_ssp460_2020_2100_depthsurf",
                   "thetao_ssp460_2020_2100_depthsurf",
                   "so_ssp460_2020_2100_depthsurf",
                   "clt_ssp460_2020_2100_depthsurf")

# Define constraints
constraints_proj <- list(time = c('2040-01-01T00:00:00Z', '2050-01-01T00:00:00Z'),
                    latitude = c(32, 43),
                    longitude = c(-126, -116))

# Extract files 
env_50_list <- map(layer_list_proj, ~{
  layers <- download_layers(dataset_id = .x, constraints = constraints_proj, fmt = "raster")
  subset(layers, grep('_mean', names(layers), value = TRUE))  # Keep only mean layers
})

