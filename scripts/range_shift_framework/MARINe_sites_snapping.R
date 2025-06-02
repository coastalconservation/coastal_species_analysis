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

# TRYING TO FIGURE THIS OUT 


# Try 1: densify coastline? might make snapping better 

# Densify the coastline so snapping is more accurate
CA_coastline_dense <- st_segmentize(CA_coastline, dfMaxLength = 100)  # segments every ~100 meters

# break up coastline linestring 
coastline_densified <- st_segmentize(CA_coastline, units::set_units(100, "m"))


coords_before <- st_coordinates(CA_coastline)
nrow(coords_before)  # Number of original vertices

# After segmentizing
CA_coastline_dense <- st_segmentize(CA_coastline, units::set_units(100, "m"))
coords_after <- st_coordinates(CA_coastline_dense)
nrow(coords_after)  # Should be much higher if it worked

# Neither of these work. the line is unchanged. 



# Try 2: horizontal lines off of the site points to get intersection with coastline linestring

# 1. For each site, extract the latitude.

# 2. Create a horizontal LINESTRING at that latitude that spans the extent of the coastline (or a bit wider).

# 3. Intersect that line with the coastline to find the point where it touches the coast.

# 4. Use that intersection point as the "snapped" point.


# Need my data to be in CRS with degree units 

coastline_wgs84 <- st_transform(CA_coastline, 4326)
sites_wgs84 <- st_transform(sites_sf, 4326)





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

# This strategy almost got us what we want, however it only resulted in 8 locations when we expected to see 137
# This script should be adapted to snap the sites correctly, however we ended up going with a workflow in ArcGIS
