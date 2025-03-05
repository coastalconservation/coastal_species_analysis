# Load required libraries
library(tidyverse)  # Includes core packages for data manipulation (e.g., dplyr, ggplot2)
library(sf)         # For spatial data handling and manipulation
library(here)       # For file path management, ensuring portability across systems

# Source external R scripts for custom functions
source(here::here('scripts', 'R', 'MarineBioClean.R'))  # Custom function for cleaning biodiversity data
source(here::here('scripts', 'range_classification', 
                  'coastline_range_edges', 'species_range_function.R'))  # Custom function for analyzing species range

# -------------------------- Load Data -------------------------- #

# Read in lat long division file 
ca_breaks <- read_csv(here('data', 'raw', 'mapping', 'CA_coast_021425.csv'))

# California state basemap 
ca_basemap <- st_read(here('data', 'raw', 'ca_state', 'CA_State.shp'))

# California coastline basemap 
ca_coastline <- st_read(here('data', 'raw', 'mapping', 'CZB', 'Statewide_CoastalZoneBoundary_Cadastral.shp'))

# Dangermond shapefile 
dangermond <- st_read(here('data', 'raw', 'dangermond_shapefile', 'jldp_boundary.shp'))

# CA coastal zone boundary 
ca_boundary <- st_read(here('data', 'raw', 'mapping', 'ds990.gdb'))

# Range Edges
range_edges <- read_csv(here("data", "processed", "range_list.csv"))

# ------------------------- Manipulation ------------------------- # 

# Assign polygon labels 
ca_boundary <- ca_boundary |>
  mutate(polygon_id = row_number())

# Filter to mainland only 
ca_mainland <- ca_boundary |> 
  filter(polygon_id == 8)

#st_crs(ca_boundary) #  NAD83 / California Albers 

# Apply 10,000 m  buffer to coastal zones 
ca_zones_extended <- st_buffer(ca_mainland, dist = 10000)#0)


# Determine bounding box of original zones for y dimensions 
ca_zones_bbox <- st_bbox(ca_mainland)
#print(ca_zones_bbox) # ymax = 450609.7 ymin = -605049.7

# Determine bounding box of extended zones for x dimensions 
extend_box <- st_bbox(ca_zones_extended)
#print(extend_box) #xmax = 308316.5 xmin = -414570.2

# Create polygon of new bounds 
crop_bbox <- st_bbox(c(
  xmin = -414570.2,
  ymin = -605049.7,
  xmax = 308316.5,
  ymax = 450609.7
), crs = st_crs(ca_zones_extended))

crop_polygon <- st_as_sfc(crop_bbox)

# Crop the extended zones using the bounding box polygon
ca_zones_crop <- st_intersection(crop_polygon, ca_zones_extended)

# Transform to WGS84 which uses degrees 
ca_zones_crop <- st_transform(ca_zones_crop, crs = "WGS84")

# Extract latitude breakpoints and sort from north to south
latitude_breaks <- sort(ca_breaks$POINT_Y, decreasing = TRUE)


# Create spliting lines from latitudes (longs from bbox of ca_zones_crop)
split_lines <- st_sfc(
  lapply(latitude_breaks, function(lat) {
    st_linestring(matrix(c(-124.8, lat, -116.7, lat), ncol = 2, byrow = TRUE))
  }),
  crs = st_crs(ca_zones_crop)
)

# Combine linestrings into single geometry 
split_lines_all <- st_union(split_lines)

# Split the coastal buffer by latitudes 
split_result <- st_split(ca_zones_crop, split_lines_all)

# Convert the resulting geometry collection into polygons
split_polygons <- st_collection_extract(split_result, "POLYGON")

# Locate center of each polygon 
centroids <- st_centroid(split_polygons)

# Create a data frame with region IDs based on latitude
ca_regions_df <- st_as_sf(split_polygons) |> 
  # Create region_id to indentify each region 
  mutate(region_id = rank(st_coordinates(centroids)[, 2]))  # Rank centroids by descending latitude

#st_write(ca_regions_df, here("data", "processed"))

# Load coastline segments data, rename columns, convert to spatial format, and add segment ID
ca_breaks <- read_csv(here('data', 'raw', 'mapping', 'CA_coast_021425.csv')) %>% 
  rename(lat = POINT_Y, long = POINT_X) %>%  # Renaming columns for clarity
  st_as_sf(coords = c("long", "lat"), crs = st_crs(ca_boundary), remove = FALSE) %>%  # Convert to sf object with correct CRS
  mutate(segment_id = 1:nrow(.))  # Add unique ID for each coastline segment

# Clean and process biodiversity data using custom function
biodiv_merge <- MarineBioClean(cbs_excel_name = 'cbs_data_2025.xlsx', 
                               point_contact_sheet = 'point_contact_summary_layered',
                               quadrat_sheet = 'quadrat_summary_data',
                               swath_sheet = 'swath_summary_data')

# Summarize biodiversity data by grouping by site, species, and year, and calculate total counts
marine_sites <- biodiv_merge %>% 
  group_by(marine_site_name, latitude)  %>% 
  summarize(marine_site_name = marine_site_name[1],
            latitude = latitude[1]) 

# Sort the latitudes of the coastline segments in ascending order
coastline_lat <- ca_breaks$lat%>% 
  sort(decreasing = FALSE) 

marine_sites_geo <- marine_sites %>%
  rowwise() %>%
  mutate(
    id = which(latitude >= coastline_lat[-length(coastline_lat)] & latitude < coastline_lat[-1])[1],
    range_lat = ifelse(!is.na(id), 
                       paste(round(coastline_lat[id], 2), round(coastline_lat[id + 1], 2), sep = "-"), 
                       NA)
  ) %>%
  ungroup() |> 
  left_join(ca_regions_df, by = c("id" = "region_id")) %>% 
  st_as_sf()

marine_sites_sum <- marine_sites_geo %>% 
  group_by(id) %>% 
  summarize(site_counts = n()) %>% 
  ungroup()

tm_shape(ca_basemap) + 
  tm_borders() + 
  # Southern Range Edges
  tm_shape(marine_sites_sum) + 
  tm_fill(col = "site_counts",
          title = "# of Marine Sites\nPer Coastal Segment",
          palette = "Greens",
          style = "cont") +
  tm_shape(marine_sites_sum) + 
  tm_borders(col = "#32292F") + 
  tm_shape(dangermond) + 
  tm_fill(col = "#705D56") + 
  tm_shape(dangermond) + 
  tm_borders(col = "#161215") +
  tm_shape(ca_coastline, bbox = ca_regions_bbox) + 
  tm_lines() + 
  tm_layout(legend.position = c(0.45, 0.65),
            legend.title.size = 2,       # Adjust legend text size
            #legend.height = -.2,
            legend.text.size = 1.5,        # Adjust legend text size
            legend.title.fontface = "bold", # Make title bold 
            inner.margins = c(0.02, 0.05, 0.1, 0.15), # Adjust margins
            frame = FALSE) 
  
