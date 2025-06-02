# Downloading Bio-ORACLE environmental rasters
# Author: Jordan Sibley 
# File created: April 23, 2025 

# -----------------------------------------------------------------------------

# This document go through the process of downloading the environmental variables
# from Bio-ORACLE, and to do some spatial pre processing to prepare them for SDMs

# Historical data time period: 2000-2010 (only goes in 10 year increments)
# Future projected data: 2050 - 2060 under SSP 460


# Environmental rasters 
# air temperature = tas 
# mixed layer depth = mlotst 
# ocean temperature = thetao 
# salinity = so 
# cloud fraction = clt 
# oxygen = o2


# -----------------------------------------------------------------------------

# Load libraries 
library(biooracler)
library(here)
library(purrr)
library(terra)
library(sf)
library(testthat)


# Read in 20 km coastal buffer shapefile from GitHUb
ca_coast <- read_sf(here('archive_data', 'CA_coastal_buffer_modeling', 'coast_buff.shp'))

# Historical data -------------------------------------------------------------

# Define layers of interest
layer_list_hist <- c("tas_baseline_2000_2020_depthsurf",
                     "o2_baseline_2000_2018_depthmax",
                     "mlotst_baseline_2000_2019_depthsurf",
                     "thetao_baseline_2000_2019_depthsurf",
                     "so_baseline_2000_2019_depthsurf",
                     "clt_baseline_2000_2020_depthsurf")

# Define constraints
# 10 year time period 
time_periods_hist <- c(2000) ## 2000: decade 2000-2010

# Latitude and Longitude for california dimensions 
latitude = c(32, 43)
longitude = c(-126, -116)

# Define time range 
time_hist = c(paste0(time_periods_hist, "-01-01T00:00:00Z"),
         paste0(time_periods_hist, "-01-01T00:00:00Z"))

constraints_hist = list(time_hist, latitude, longitude)
names(constraints_hist) = c("time", "latitude", "longitude")

# Extract files 
env_00_list <- map(layer_list_hist, ~{
  layers <- download_layers(dataset_id = .x, constraints = constraints_hist, fmt = "raster")
  subset(layers, grep('_mean', names(layers), value = TRUE))  # Keep only mean layers
})

# Test that the rasters are compatible for stacking 
test_that("All rasters have matching resolution, extent, and CRS", {
  ref_raster <- env_00_list[[1]]
  
  # Check resolution
  res_match <- map_lgl(env_00_list, ~ identical(res(.x), res(ref_raster)))
  expect_true(all(res_match), info = "Not all rasters have the same resolution")
  
  # Check extent
  ext_match <- map_lgl(env_00_list, ~ identical(ext(.x), ext(ref_raster)))
  expect_true(all(ext_match), info = "Not all rasters have the same extent")
  
  # Check CRS
  crs_match <- map_lgl(env_00_list, ~ identical(crs(.x), crs(ref_raster)))
  expect_true(all(crs_match), info = "Not all rasters have the same CRS")
})

# The extents do not match, but they will once they have been masked to the coastal buffer

# Mask historical rasters to coastline buffer ----------------------------------

# Check that CRS of env_00 rasters match ca coastline buffer shapefile 
if(st_crs(ca_coast) == st_crs(env_00_list[[1]])) {
  print("The coordinate reference systems match")
} else {
  warning("The coordinate reference systems were not a match. Transformation has now occured")
  ca_coast <- st_transform(ca_coast, st_crs(env_00_list[[1]]))
}

# Crop and mask hist rasters to ca_coast shapefile 
env_00_list <- lapply(env_00_list, function(r) {
  terra::mask(terra::crop(r, ca_coast), ca_coast)
})


# Projected data -------------------------------------------------------------

# Define layers for future projected data under SSP 4.6 
layer_list_proj <- c("tas_ssp460_2020_2100_depthsurf",
                   "o2_ssp460_2020_2100_depthmax",
                   "mlotst_ssp460_2020_2100_depthsurf",
                   "thetao_ssp460_2020_2100_depthsurf",
                   "so_ssp460_2020_2100_depthsurf",
                   "clt_ssp460_2020_2100_depthsurf")

# 10 year time period 
time_periods_proj <- c(2050) ## 2050: decade 2050-2060

# Define time range 
time_proj = c(paste0(time_periods_proj, "-01-01T00:00:00Z"),
              paste0(time_periods_proj, "-01-01T00:00:00Z"))

constraints_proj = list(time_proj, latitude, longitude)
names(constraints_proj) = c("time", "latitude", "longitude")
# Extract files 
env_50_list <- map(layer_list_proj, ~{
  layers <- download_layers(dataset_id = .x, constraints = constraints_proj, fmt = "raster")
  subset(layers, grep('_mean', names(layers), value = TRUE))  # Keep only mean layers
})


# Test that the rasters are compatible for stacking 
test_that("All rasters have matching resolution, extent, and CRS", {
  ref_raster <- env_50_list[[1]]
  
  # Check resolution
  res_match <- map_lgl(env_50_list, ~ identical(res(.x), res(ref_raster)))
  expect_true(all(res_match), info = "Not all rasters have the same resolution")
  
  # Check extent
  ext_match <- map_lgl(env_50_list, ~ identical(ext(.x), ext(ref_raster)))
  expect_true(all(ext_match), info = "Not all rasters have the same extent")
  
  # Check CRS
  crs_match <- map_lgl(env_50_list, ~ identical(crs(.x), crs(ref_raster)))
  expect_true(all(crs_match), info = "Not all rasters have the same CRS")
})

# The extents do not match, but they will once they have been masked to the coastal buffer

# Mask projected rasters to coastline buffer ----------------------------------

# Check that CRS of env_00 rasters match ca coastline buffer shapefile 
if(st_crs(ca_coast) == st_crs(env_50_list[[1]])) {
  print("The coordinate reference systems match")
} else {
  warning("The coordinate reference systems were not a match. Transformation has now occured")
  ca_coast <- st_transform(ca_coast, st_crs(env_50_list[[1]]))
}

# Crop and mask hist rasters to ca_coast shapefile 
env_50_list <- lapply(env_50_list, function(r) {
  terra::mask(terra::crop(r, ca_coast), ca_coast)
})


# Download raster files and upload to Cyberduck -------------------------------

# Define path to historical bio oracle folder on workbench 2 
outpath_hist <- file.path("/capstone/coastalconservation/data/raw/BioOrc_rasters/historical")

# Write out each raster
for (i in seq_along(env_00_list)) {
  terra::writeRaster(
    env_00_list[[i]],
    filename = file.path(outpath_hist, paste0(names(env_00_list[[i]]), "_00.tif")),
    overwrite = TRUE,
    filetype = "GTiff"
  )
}

# Define path to projected bio oracle folder on workbench 2 
outpath_proj <- file.path("/capstone/coastalconservation/data/raw/BioOrc_rasters/projected_ssp460")

# Write out each raster
for (i in seq_along(env_50_list)) {
  terra::writeRaster(
    env_50_list[[i]],
    filename = file.path(outpath_proj, paste0(names(env_50_list[[i]]), "_50.tif")),
    overwrite = TRUE,
    filetype = "GTiff"
  )
}
