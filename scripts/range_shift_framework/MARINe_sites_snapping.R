# Snapping MARINe California Sites to Coastline 
# Author: Jordan Sibley 
# File created: April 30, 2025 

# -----------------------------------------------------------------------------

# The purpose of this document is to snap the CA MARINe sites to a coastal polyline
# in order to a dataset to calculate the distance between sites and the southern 
# California border in ArcGIS

# ------------------------------------------------------------------------------

# Load required libraries
library(tidyverse)  
library(sf)         
library(here)       
library(tmap)
#library(lwgeom)



# Read in data ----------------------------------------------------------------
# Read in MARINe site names and lat/lon points 
sites <- read_csv('/capstone/coastalconservation/data/raw/MARINe_data/sites/marine_sites_CA.csv')

# Read in CA coastline 
coastline <- read_sf('/capstone/coastalconservation/data/raw/spatial_data/CA_coastline_polyline/coast.line.CA.shp')


# Prep data -------------------------------------------------------------------
# CA COASTLINE 
# Check CA coastline 
plot(coastline)
st_crs(coastline) # no CRS 

# Assign a CRS to coastline file (NAD83 / California Albers)
CA_coastline <- st_set_crs(coastline, 3310)

# Check new CRS 
st_crs(CA_coastline) # new CRS has been set 


# MARINE SITES 

# Make MARINe sites an sf object to include geometry 
sites_sf <- st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326)  # WGS84 for degree points

# Convert CRS to match ca coastline (NAD83 / California Albers)
sites_sf <- st_transform(sites_sf, 3310)

# Check that the CRS of each object matches 
if(st_crs(CA_coastline) == st_crs(sites_sf)) {
  print("The coordinate reference systems match")
} else {
  warning("The coordinate reference systems were not a match. Transformation has now occured")
  sites_sf <- st_transform(sites_sf, st_crs(CA_coastline))
}


# Snap MARINe sites to coastline -----------------------------------------------

# Densify the coastline so snapping is more accurate
CA_coastline_dense <- st_segmentize(CA_coastline, dfMaxLength = 100)  # segments every ~100 meters


CA_coastline_dense_segments <- CA_coastline_dense %>%
  st_segmentize(units::set_units(100, "m")) %>%   # 100m segment resolution
  st_cast("LINESTRING") 

CA_coastline_segments <- CA_coastline_dense %>%
  st_cast("LINESTRING") %>%   # Break MULTILINESTRING into individual LINESTRINGs
  st_segmentize(units::set_units(100, "m")) 


nrow(CA_coastline_dense_segments)  # should be >> 8

# Check that the CRS of each object matches 
if(st_crs(CA_coastline_dense) == st_crs(sites_sf)) {
  print("The coordinate reference systems match")
} else {
  warning("The coordinate reference systems were not a match. Transformation has now occured")
  CA_coastline_dense <- st_transform(CA_coastline_dense, st_crs(sites_sf))
}


# Snap sites to the nearest point *along the actual line*
sites_snapped_TEST <- sites_sf %>%
  rowwise() %>%
  mutate(
    snapped_geometry = {
      nearest_line <- st_nearest_points(geometry, CA_coastline_dense)
      st_cast(nearest_line, "POINT")[2]
    }
  ) %>%
  ungroup() %>%
  st_as_sf() %>%
  mutate(geometry = snapped_geometry) %>%
  select(marine_site_name, geometry)

# CHECK 
n_distinct(sites_sf$marine_site_name)  # should be 108
n_distinct(st_geometry(sites_snapped_TEST)) # should also be 108


# -----------------------------------------------------------------------------
sites_snapped <- sites_sf %>%  
  mutate(nearest_id = st_nearest_feature(geometry, CA_coastline)) %>%  # find nearest distance between point and line
  rowwise() %>% 
  mutate(
    snapped_geometry = st_nearest_points(geometry, CA_coastline[nearest_id, ]) %>% 
      st_cast("POINT") %>%  # create new point
      {.[2]} # select only new point as geometry 
  ) %>% 
  ungroup() %>% 
  st_as_sf() %>% 
  mutate(geometry = snapped_geometry) %>%  # rewrite geometry column with new points
  select(marine_site_name, geometry) # only select marine_site_name, geometry 

# Check & Export ---------------------------------------------------------------
# View new snapped points 
tm_shape(CA_coastline) +
  tm_lines(col = "blue", lwd = 2) +                 
  tm_shape(sites_snapped) +
  tm_symbols(col = "red", size = 0.5) +             
  tm_layout(title = "Snapped Marine Sites on California Coastline")
  
# ONLY 8 POINTS


# Ensure no sites were lost 
all(sites$marine_site_name == sites_snapped$marine_site_name) # TRUE

# Check for duplicated site locations 
any(duplicated(st_geometry(sites_snapped))) # There are some repeated geometries 

# Check which sites have repeated geometries 
print(unique(sites_snapped$geometry)) # it seems there are only 8 locations 


# Export files to processed folder 
#st_write(CA_coastline, '/capstone/coastalconservation/data/processed/MARINe_site_snapping/CA_coastline.shp')
#st_write(sites_snapped, '/capstone/coastalconservation/data/processed/MARINe_site_snapping/MARINe_sites_snapped.shp')


