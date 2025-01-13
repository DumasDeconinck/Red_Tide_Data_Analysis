#load libraries
library(ggmap)
library(readxl)
library(ggspatial)
library(ggrepel)
library(RColorBrewer)
library(osmdata)
library(sf)
library(dplyr)
library(ggnewscale)

library(rnaturalearth)
library(rnaturalearthhires)
###Alternative way of plotting
# Download the shapefile for China
china_shape <- ne_countries(scale = 10, returnclass = "sf")

# Filter the region for Hong Kong
hk_shape <- china_shape[china_shape$name == "Hong Kong", ]

# Plot the map and overlay the polygon shape
map_plot <- ggplot() +
  geom_sf(data = china_shape, fill = "white") +
  geom_sf(data = hk_shape, fill = "grey") +
  geom_point(data = df_Species, aes(x = lon, y = lat), size = 2) +
  scale_colour_manual(values = brewer.pal(8, "Dark2")) +
  coord_sf(xlim = c(113.78, 114.5101), ylim = c(22.13, 22.5667)) +
  theme_void() + 
  theme(panel.background = element_rect(fill = "blue"))

# Display the map plot
map_plot

#############continued way
#Load map
#StadiaMAP API key
ggmap::register_stadiamaps("9b87c6a6-dbdf-4f7e-8da6-92f2b28c72b0") #You have to create a stadia account and create your own API key. This key is personal, do not share it.

HK_locations <- read.csv("WoRMSCorrected_redtides.csv")
df = read.csv("WoRMSCorrected_redtides.csv")

# Manually define the bounding box coordinates for Hong Kong. You can use getbbox("hong_kong"), but that code seems to change on the day...
x_min <- c(113.7368, min(df$lon)-0.01)[which.min(c(113.7368, min(df$lon)-0.01))] #Make sure that the sightings fit
x_max <- c(114.5101, max(df$lon)+0.01)[which.max(c(114.5101, max(df$lon)+0.01))] 
y_min <- c(22.1533, min(df$lat)-0.01)[which.min(c(22.1533, min(df$lat)-0.01))] 
y_max <- c(22.5667, max(df$lat)+0.01)[which.max(c(114.5101, max(df$lat)+0.01))]

x_min <- 113.78
x_max <- 114.5101
y_min <- 22.13
y_max <- 22.5667


HK_map <- get_stadiamap(bbox = c(x_min, y_min, x_max, y_max), zoom = 11, maptype = "alidade_smooth") #Zoom affects how long you load Quality vs time!

#Plot1: All sightings
#By group
map_plot <- ggmap(HK_map) +
  geom_point(data = HK_locations, aes(x = lon, y = lat, color = Group), size = 2) +
  scale_colour_manual(values = brewer.pal(8, "Dark2")) +
  coord_map() # Use coord_map instead of coord_cartesian to maintain the map's aspect ratio

#Specify the bounding box coordinates for the scale bar. This part of the script is only required when using getbbox("hong_kong")
#bbox <- attr(HK_map, "bb")
#x_min <- bbox$ll.lon
#x_max <- bbox$ur.lon
#y_min <- bbox$ll.lat
#y_max <- bbox$ur.lat

scale_bar <- ggsn::scalebar(
  location = "bottomright",
  dist = 10,
  st.size = 2,
  height = 0.015,
  x.min = x_min,
  x.max = x_max,
  y.min = y_min,
  y.max = y_max,
  transform = TRUE,
  model = "WGS84",
  dist_unit = "km"
) # Add scale bar

north_arrow <- annotation_north_arrow(
  location = "topleft",
  which_north = "true",
  pad_x = unit(0.5, "cm"),
  pad_y = unit(0.5, "cm"),
  width = unit(0.9, "cm") # Adjust the width of the north arrow here
) # Add north arrow

#Add text annotation on the right side of the plot
scale_text <- data.frame(
  x = x_max - 0.035,
  y = y_min + 0.01,
  label = "10 KM"
)

final_plot <- map_plot + scale_bar + north_arrow +
  annotate(
    "text",
    x = scale_text$x,
    y = scale_text$y,
    label = scale_text$label,
    size = 3,
    hjust = 1, # Set hjust to 1 for right alignment
    vjust = 0
  )

print(final_plot)

#by order (top 8)
unique(df$Order)
# Calculate the count of each species
order_count <- df %>%
  count(Order) %>%
  arrange(desc(n))

# Get the top 8 orders
top_8_order <- head(order_count$Order, 8)

df_Order <- df %>%
  filter(Order %in% top_8_order)

map_plot <- ggmap(HK_map) +
  geom_point(data = df_Order, aes(x = lon, y = lat, color = Order), size = 2) +
  scale_colour_manual(values = brewer.pal(8, "Dark2")) +
  coord_map() # Use coord_map instead of coord_cartesian to maintain the map's aspect ratio

#Specify the bounding box coordinates for the scale bar. This part of the script is only required when using getbbox("hong_kong")
#bbox <- attr(HK_map, "bb")
#x_min <- bbox$ll.lon
#x_max <- bbox$ur.lon
#y_min <- bbox$ll.lat
#y_max <- bbox$ur.lat

scale_bar <- ggsn::scalebar(
  location = "bottomright",
  dist = 10,
  st.size = 2,
  height = 0.015,
  x.min = x_min,
  x.max = x_max,
  y.min = y_min,
  y.max = y_max,
  transform = TRUE,
  model = "WGS84",
  dist_unit = "km"
) # Add scale bar

north_arrow <- annotation_north_arrow(
  location = "topleft",
  which_north = "true",
  pad_x = unit(0.5, "cm"),
  pad_y = unit(0.5, "cm"),
  width = unit(0.9, "cm") # Adjust the width of the north arrow here
) # Add north arrow

#Add text annotation on the right side of the plot
scale_text <- data.frame(
  x = x_max - 0.035,
  y = y_min + 0.01,
  label = "10 KM"
)

final_plot <- map_plot + scale_bar + north_arrow +
  annotate(
    "text",
    x = scale_text$x,
    y = scale_text$y,
    label = scale_text$label,
    size = 3,
    hjust = 1, # Set hjust to 1 for right alignment
    vjust = 0
  )

print(final_plot)


#by Phamily (top 8)
unique(df$Phamily)
# Calculate the count of each species
family_count <- df %>%
  count(Phamily) %>%
  arrange(desc(n))

# Get the top 8 orders
top_8_family <- head(family_count$Phamily, 8)

df_family <- df %>%
  filter(Phamily %in% top_8_family)

map_plot <- ggmap(HK_map) +
  geom_point(data = df_family, aes(x = lon, y = lat, color = Phamily), size = 2) +
  scale_colour_manual(values = brewer.pal(8, "Dark2")) +
  coord_map() # Use coord_map instead of coord_cartesian to maintain the map's aspect ratio

#Specify the bounding box coordinates for the scale bar. This part of the script is only required when using getbbox("hong_kong")
#bbox <- attr(HK_map, "bb")
#x_min <- bbox$ll.lon
#x_max <- bbox$ur.lon
#y_min <- bbox$ll.lat
#y_max <- bbox$ur.lat

scale_bar <- ggsn::scalebar(
  location = "bottomright",
  dist = 10,
  st.size = 2,
  height = 0.015,
  x.min = x_min,
  x.max = x_max,
  y.min = y_min,
  y.max = y_max,
  transform = TRUE,
  model = "WGS84",
  dist_unit = "km"
) # Add scale bar

north_arrow <- annotation_north_arrow(
  location = "topleft",
  which_north = "true",
  pad_x = unit(0.5, "cm"),
  pad_y = unit(0.5, "cm"),
  width = unit(0.9, "cm") # Adjust the width of the north arrow here
) # Add north arrow

#Add text annotation on the right side of the plot
scale_text <- data.frame(
  x = x_max - 0.035,
  y = y_min + 0.01,
  label = "10 KM"
)

final_plot <- map_plot + scale_bar + north_arrow +
  annotate(
    "text",
    x = scale_text$x,
    y = scale_text$y,
    label = scale_text$label,
    size = 3,
    hjust = 1, # Set hjust to 1 for right alignment
    vjust = 0
  )

print(final_plot)

#by Genus (top 8)
# Calculate the count of each species
genus_count <- df %>%
  count(Genus) %>%
  arrange(desc(n))

# Get the top 8 orders
top_8_genus <- head(genus_count$Genus, 8)

df_Genus <- df %>%
  filter(Genus %in% top_8_genus)

map_plot <- ggmap(HK_map) +
  geom_point(data = df_Genus, aes(x = lon, y = lat, color = Genus), size = 2) +
  scale_colour_manual(values = brewer.pal(8, "Dark2")) +
  coord_map() # Use coord_map instead of coord_cartesian to maintain the map's aspect ratio

#Specify the bounding box coordinates for the scale bar. This part of the script is only required when using getbbox("hong_kong")
#bbox <- attr(HK_map, "bb")
#x_min <- bbox$ll.lon
#x_max <- bbox$ur.lon
#y_min <- bbox$ll.lat
#y_max <- bbox$ur.lat

scale_bar <- ggsn::scalebar(
  location = "bottomright",
  dist = 10,
  st.size = 2,
  height = 0.015,
  x.min = x_min,
  x.max = x_max,
  y.min = y_min,
  y.max = y_max,
  transform = TRUE,
  model = "WGS84",
  dist_unit = "km"
) # Add scale bar

north_arrow <- annotation_north_arrow(
  location = "topleft",
  which_north = "true",
  pad_x = unit(0.5, "cm"),
  pad_y = unit(0.5, "cm"),
  width = unit(0.9, "cm") # Adjust the width of the north arrow here
) # Add north arrow

#Add text annotation on the right side of the plot
scale_text <- data.frame(
  x = x_max - 0.035,
  y = y_min + 0.01,
  label = "10 KM"
)

final_plot <- map_plot + scale_bar + north_arrow +
  annotate(
    "text",
    x = scale_text$x,
    y = scale_text$y,
    label = scale_text$label,
    size = 3,
    hjust = 1, # Set hjust to 1 for right alignment
    vjust = 0
  )

print(final_plot)

#by Species (top 8)
# Calculate the count of each species
species_count <- df %>%
  count(Species) %>%
  arrange(desc(n))

# Get the top 8 orders
top_8_species <- head(species_count$Species, 8)

df_Species <- df %>%
  filter(Species %in% top_8_species)

HK_map <- get_stadiamap(bbox = c(x_min, y_min, x_max, y_max), zoom = 11, maptype = "stamen_terrain_background") #Zoom affects how long you load Quality vs time!

map_plot <- ggmap(HK_map) +
  geom_point(data = df_Species, aes(x = lon, y = lat, color = Species), size = 2) +
  scale_colour_manual(values = brewer.pal(8, "Dark2")) +
  coord_map() 

#Specify the bounding box coordinates for the scale bar. This part of the script is only required when using getbbox("hong_kong")
#bbox <- attr(HK_map, "bb")
#x_min <- bbox$ll.lon
#x_max <- bbox$ur.lon
#y_min <- bbox$ll.lat
#y_max <- bbox$ur.lat

scale_bar <- ggsn::scalebar(
  location = "bottomright",
  dist = 10,
  st.size = 2,
  height = 0.015,
  x.min = x_min,
  x.max = x_max,
  y.min = y_min,
  y.max = y_max,
  transform = TRUE,
  model = "WGS84",
  dist_unit = "km"
) # Add scale bar

north_arrow <- annotation_north_arrow(
  location = "topleft",
  which_north = "true",
  pad_x = unit(0.5, "cm"),
  pad_y = unit(0.5, "cm"),
  width = unit(0.9, "cm") # Adjust the width of the north arrow here
) # Add north arrow

#Add text annotation on the right side of the plot
scale_text <- data.frame(
  x = x_max - 0.035,
  y = y_min + 0.01,
  label = "10 KM"
)

final_plot <- map_plot + scale_bar + north_arrow +
  annotate(
    "text",
    x = scale_text$x,
    y = scale_text$y,
    label = scale_text$label,
    size = 3,
    hjust = 1, # Set hjust to 1 for right alignment
    vjust = 0
  )

print(final_plot)

#Polygon
Polydat = read.csv("EPD WCZ (9).csv") #drew this map with google, my maps. The data needs to be converted from csv to r

#create empty dataframe
WCZ = data.frame()

for (i in 1:nrow(Polydat)){
  # Define the polygon coordinates
  polygon <- Polydat[i,1]
  
  # Extract the coordinates
  coordinates <- gsub("POLYGON \\(\\(|\\)", "", polygon)
  coordinates <- strsplit(coordinates, ", ")[[1]]
  
  # Split the coordinates into lon and lat
  strsplit(coordinates[2], "\\s+")
  
  lon <- numeric()
  lat <- numeric()
  for (j in 1:length(coordinates)) {
    coord_parts <- strsplit(coordinates[j], "\\s+")
    lon <- c(lon, as.numeric(coord_parts[[1]][1]))
    lat <- c(lat, as.numeric(coord_parts[[1]][2]))
  }
  # Create the data frame
  Tempdat <- data.frame(
    lon = c(lon, lon[1]),
    lat = c(lat, lat[1]),
    wcz = Polydat[i,2]
  )
  WCZ = rbind(WCZ, Tempdat)
}
WCZ = na.omit(WCZ)

HK_map <- get_stadiamap(bbox = c(x_min, y_min, x_max, y_max), zoom = 13, maptype = "stamen_terrain_background") #Zoom affects how long you load Quality vs time!

map_plot <- ggmap(HK_map) +
  geom_polygon(data = WCZ, aes(x = lon, y = lat,  fill = wcz), colour = NA, size = 0.1, alpha = 1) +
  scale_fill_manual(values = brewer.pal(10, "Set3")) +
  new_scale_fill() + 
  geom_point(data = HK_locations, aes(x = lon, y = lat, bg = Group), col = "white", pch = 21, size = 1.5) +
  scale_fill_manual(values = c(brewer.pal(8, "Dark2")[1], brewer.pal(8, "Dark2")[2], brewer.pal(8, "Dark2")[8])) +
  coord_map() +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        legend.title = element_blank())

scale_bar <- ggsn::scalebar(
  location = "bottomright",
  dist = 10,
  st.size = 2,
  height = 0.015,
  x.min = x_min,
  x.max = x_max,
  y.min = y_min,
  y.max = y_max,
  transform = TRUE,
  model = "WGS84",
  dist_unit = "km"
) # Add scale bar

north_arrow <- annotation_north_arrow(
  location = "topleft",
  which_north = "true",
  pad_x = unit(0.5, "cm"),
  pad_y = unit(0.5, "cm"),
  width = unit(0.9, "cm") # Adjust the width of the north arrow here
) # Add north arrow

#Add text annotation on the right side of the plot
scale_text <- data.frame(
  x = x_max - 0.035,
  y = y_min + 0.01,
  label = "10 KM"
)

final_plot <- map_plot + scale_bar + north_arrow +
  annotate(
    "text",
    x = scale_text$x,
    y = scale_text$y,
    label = scale_text$label,
    size = 3,
    hjust = 1, # Set hjust to 1 for right alignment
    vjust = 0
  )

print(final_plot)

#Create amap without WCZ to overlay on top to only have WCZ in water.
HK_map <- get_stadiamap(bbox = c(x_min, y_min, x_max, y_max), zoom = 12, maptype = "stamen_toner_background") #Zoom affects how long you load Quality vs time!
ggmap(HK_map) +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        legend.title = element_blank()) +
  coord_map() # Use coord_map instead of coord_cartesian to maintain the map's aspect ratio

# Calculate density
density_map <- kde2d(HK_locations$lon, HK_locations$lat, n = 100)

# Create a data frame for the density values
density_df <- expand.grid(lon = density_map$x, lat = density_map$y)
density_df$density <- as.vector(density_map$z)

# Plot the map with density-based color gradient
ggmap(HK_map) +
  geom_tile(data = density_df, aes(x = lon, y = lat, fill = density), alpha = density_df$density) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  theme(axis.title = element_blank(), legend.title = element_blank()) +
  coord_map()

########## WARNING: USE EPD WCZ (7).csv. This is bigger and includes Shenzhen borders. Some samples may not be included when using 9.
#Extract the Water Control Zone location from the longitudes and latitudes
Polydat = read.csv("EPD WCZ (7).csv") 

# Convert Polydat dataframe to an sf object
Polydat_sf <- st_as_sf(Polydat, wkt = "WKT")

# Convert df dataframe to an sf object
df_sf <- st_as_sf(df, coords = c("lon", "lat"))

# Check and transform CRS if needed
if (!identical(st_crs(Polydat_sf), st_crs(df_sf))) {
  df_sf <- st_transform(df_sf, crs = st_crs(Polydat_sf))
}

# Perform a spatial join, specifying the attribute columns to retain
good_points <- st_join(df_sf,Polydat_sf)

df$WCZ = good_points$name

write.csv(df, file = "Red_tide_and_WCZ_02162024.csv")

#Maps of most common species
df = read.csv(file = "Red_tide_and_WCZ_02162024.csv")
# Calculate the count of each species
species_count <- df %>%
  count(Species) %>%
  arrange(desc(n))

#calculate top 8
top_8_species <- head(subset(species_count, !is.na(species_count$Species)), 7)

df$Species <- ifelse(df$Species %in% top_8_species$Species, df$Species, "Other")

df_Species <- df %>%
  group_by(WCZ, Species) %>%
  summarize(count = n())

df_relative <- df_Species %>%
  group_by(WCZ) %>%
  mutate(Relative_Count = count / sum(count)) %>%
  ungroup()

#get centroids
centroids = st_centroid(Polydat_sf)
centroids$lon = gsub("POINT \\(\\(|\\)", "", centroids$WKT)

#Noctiluca scintillans
NScintillans = subset(df_relative, Species == "Noctiluca scintillans")
