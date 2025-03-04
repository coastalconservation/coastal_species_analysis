library(tidyverse)
library(here)
library(sf)
library(tmap)
library(lwgeom) # for st_split

# -------------------------- Load Data -------------------------- #

# Read in lat long division file 
ca_breaks <- read_csv(here('data', 'raw', 'mapping', 'CA_coast_021425.csv'))

# California coastline basemap 
ca_coastline <- st_read(here('data', 'raw', 'tl_2019_us_coastline', 'tl_2019_us_coastline.shp'))

# Filter to pacific only 
ca_coastline <- ca_coastline |> 
  filter(NAME == "Pacific")

# California state basemap 
ca_basemap <- st_read(here('data', 'raw', 'ca_state', 'CA_State.shp'))

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

# Transform to match 
ca_coastline <- st_transform(ca_coastline, crs = st_crs(ca_regions_df))
ca_basemap <- st_transform(ca_basemap, crs = st_crs(ca_regions_df))
dangermond <- st_transform(dangermond, crs = st_crs(ca_regions_df))

# Bounding box to ca 100 regions 
ca_regions_bbox <- st_bbox(ca_regions_df)

# Nothern Range Edges
all_range_edges <- range_edges |> 
  filter(range_edge_category  %in% c("Northern Range Edge",
                                     "Southern Range Edge",
                                     "Endemic Presence")) %>% 
  group_by(id, range_edge_category) %>% 
  summarise(species_counts = n()) %>% 
  ungroup()

range_edges_merge <- all_range_edges |> 
  left_join(ca_regions_df, by = c("id" = "region_id")) %>% 
  st_as_sf()

edge16 <- range_edges_merge |> 
  filter(id == 16)

south_range_edges <- range_edges_merge %>% 
  filter(range_edge_category == "Southern Range Edge") %>%
  filter(id  %in% seq(3,17)) %>%  
  rbind(edge16) 
  
south_range_edges[south_range_edges$id == 16,
                  "range_edge_category"] <- "Southern Range Edge"
south_range_edges[south_range_edges$id == 16,
                  "species_counts"] <- 0

north_range_edges <- range_edges_merge %>% 
  filter(range_edge_category == "Northern Range Edge") %>%
  filter(id  %in% seq(2,16)) 

dangermond_range_edges <- range_edges_merge %>% 
  filter(id  %in% seq(6,7))

# -------------------------- Mapping -------------------------- #
# View basemap 

# tm_shape(ca_basemap) + 
#   tm_borders() + 
# tm_shape(range_edges_merge) + 
#   tm_fill(col = "species_counts", 
#           title = "# Range Edges", 
#           palette = "Reds") +
# tm_shape(range_edges_merge) + 
#   tm_borders(col = "grey40") + 
# tm_shape(dangermond) + 
#   tm_fill(col = "blue") + 
# tm_shape(ca_coastline, bbox = ca_regions_bbox) + 
#   tm_lines() + 
#   tm_layout(legend.title.size = 1.5,     # Adjust legend title size
#             legend.text.size = 1,        # Adjust legend text size
#             legend.title.fontface = "bold") + # Make title bold 
#   tm_scale_bar(position = c(0.02, 0.02)) + # scale bar
#   tm_compass(position = c(0.01, 0.08), text.size = 0.5) # compass

nre_plot <- tm_shape(ca_basemap) + 
  tm_borders() + 
  # Northern Range Edges
tm_shape(north_range_edges) +   
  tm_fill(col = "species_counts", 
          title = "# of Taxon with\nNorthern Range Edges",
          palette = colorRampPalette(c("#DEF5E5FF", "#96DDB5FF", "#49C1ADFF"))(3),
          breaks=c(0, 5, 10, 15)) +
  tm_shape(north_range_edges) + 
  tm_borders(col = "#32292F") + 
  tm_shape(dangermond_range_edges) +
  tm_borders(col = "#161215") +
  tm_shape(dangermond) + 
  tm_fill(col = "#705D56") + 
  tm_shape(ca_coastline, bbox = ca_basemap) + 
  tm_lines() + 
  tm_layout(legend.title.size = 1.5,     # Adjust legend title size
            legend.text.size = 1,        # Adjust legend text size
            legend.title.fontface = "bold", # Make title bold 
            inner.margins = c(0.02, 0.05, 0.1, 0.1), # Adjust margins
            frame = FALSE) #+ 
  #tm_scale_bar(position = c(0.02, 0.02), text.size = 3) + # scale bar
  #tm_compass(position = c(0.01, 0.08), text.size = .5) # compass

sre_plot <- tm_shape(ca_basemap) + 
  tm_borders() + 
# Southern Range Edges
tm_shape(south_range_edges) + 
  tm_fill(col = "species_counts",
          title = "# of Taxon with\nSouthern Range Edges",
          palette = colorRampPalette(c("#359EAAFF",  "#3B5698FF", "#2B1C35FF"))(3),
          breaks = c(0, 15, 30, 45)) +
  tm_shape(south_range_edges) + 
  tm_borders(col = "#32292F") + 
  tm_shape(dangermond_range_edges) +
  tm_borders(col = "#161215") +
  tm_shape(dangermond) + 
  tm_fill(col = "#705D56") + 
  tm_shape(ca_coastline, bbox = ca_regions_bbox) + 
  tm_lines() + 
  tm_layout(legend.title.size = 1.5,     # Adjust legend title size
            legend.text.size = 1,        # Adjust legend text size
            legend.title.fontface = "bold", # Make title bold 
            inner.margins = c(0.02, 0.05, 0.1, 0.1), # Adjust margins
            frame = FALSE) #+ # Make title bold 
  #tm_scale_bar(position = c(0.02, 0.02), text.size = 3) + # scale bar
  #tm_compass(position = c(0.01, 0.08), text.size = .5) # compass

tmap_save(filename=here("plots", "figures", "nre_plot.png"), tm=nre_plot, dpi=600)
tmap_save(filename=here("plots", "figures", "sre_plot.png"), tm=sre_plot, dpi=600)
