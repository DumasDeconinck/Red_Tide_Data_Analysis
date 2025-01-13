#MAP2
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

library(MASS)

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

HK_map <- get_stadiamap(bbox = c(x_min, y_min, x_max, y_max), zoom = 14, maptype = "stamen_terrain_background") #Zoom affects how long you load Quality vs time!

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

#Empty MAP
map_plot <- ggmap(HK_map) +
  #geom_point(data = HK_locations, aes(x = lon, y = lat, colour = Group), size = 1) +
  #scale_fill_manual(values = c(brewer.pal(8, "Dark2")[1], brewer.pal(8, "Dark2")[2], brewer.pal(8, "Dark2")[8])) +
  #scale_colour_manual(values = c("black", "white", "yellow")) +  
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

# Calculate density
density_map <- kde2d(HK_locations$lon, HK_locations$lat, n = 200, lims = c(x_min, x_max, y_min, y_max))

# Create a data frame for the density values# Create a data frame for the density values# Create a data frame for the density values
density_df <- expand.grid(lon = density_map$x, lat = density_map$y)
density_df$density <- as.vector(density_map$z)

# Plot the map with density-based color gradient
HK_heatmap = ggmap(HK_map) +
  geom_tile(data = density_df, aes(x = lon, y = lat, fill = density), alpha = density_df$density) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "Reds")) +
  #scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  theme(axis.title = element_blank(), legend.title = element_blank()) +
  coord_map()
HK_heatmap

HK_heatmap = ggmap(HK_map) +
  stat_density2d(data = HK_locations, aes(x = lon, y = lat, fill = after_stat(..level..), alpha = after_stat(..level..)), contour_var = "density", geom = "polygon", size = 0.01, bins = 30) +
  scale_fill_gradient(low = "yellow", high = "red") + 
  scale_alpha(range = c(0.2,0.7), guide = "none") + 
  theme_minimal() +
  theme(axis.title = element_blank(), legend.title = element_blank()) +
  coord_map()
HK_heatmap

HK_heatmap = ggmap(HK_map) +
  stat_density2d(data = HK_locations, aes(x = lon, y = lat, fill = after_stat(..level..), alpha = after_stat(..level..)), contour_var = "density", geom = "polygon", size = 0.01, bins = 30) +
  scale_fill_gradient(low = "yellow", high = "red",
                      breaks = c(0, 10, 20, 30, 40, 50),
                      labels = c("100", "80", "60", "40", "20", "0")) +
  scale_alpha(range = c(0.2,0.7), guide = "none") +
  theme_minimal() +
  theme(axis.title = element_blank(), legend.title = element_blank()) +
  coord_map()

HK_heatmap

library(tidyverse)
library(sp)

h = c(MASS::bandwidth.nrd(HK_locations$lon), MASS::bandwidth.nrd(HK_locations$lat))
dens = kde2d(HK_locations$lon, HK_locations$lat, n = 100, lims = c(x_min, x_max, y_min, y_max))
zdf = data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z))

breaks = pretty(range(zdf$z), 5)

z = tapply(zdf$z, zdf[c("x","y")], identity)

cl = grDevices::contourLines(
  x = sort(unique(dens$x)), y = sort(unique(dens$y)), z = dens$z,
  levels = breaks
)

SpatialPolygons(
  lapply(1:length(cl), function(idx) {
    Polygons(
      srl = list(Polygon(
        matrix(c(cl[[idx]]$x, cl[[idx]]$y), nrow=length(cl[[idx]]$x), byrow=FALSE)
      )),
      ID = idx
    )
  })
) -> cont

coordinates(HK_locations) <- ~lon+lat

df = data_frame(
  ct = sapply(over(cont, geometry(HK_locations), returnList = TRUE), length),
  id = 1:length(ct),
  lvl = sapply(cl, function(x) x$level)
) %>% 
  count(lvl, wt=ct) %>% 
  mutate(
    pct = n/length(HK_locations),
    pct_lab = sprintf("%s of the points fall within this level", scales::percent(pct))
  )
df

percentiles = df$pct * 100
percentiles = paste(round(percentiles, 2), "%", sep = "")

HK_heatmap = ggmap(HK_map) +
  stat_density2d(data = HK_locations, aes(x = lon, y = lat, fill = after_stat(..level..), alpha = after_stat(..level..)), contour_var = "density", geom = "polygon", size = 0.01, bins = 30) +
  scale_fill_gradient(low = "#FFD23C", high = "red",
                      breaks = c(10, 20, 30, 40, 50),
                      labels = percentiles) +
  scale_alpha(range = c(0,0), guide = "none") +
  theme_minimal() +
  theme(axis.title = element_blank(), legend.position = "none") +
  labs(fill = "Points\nwithin\nlevel") +
  coord_map()

HK_heatmap

final_plot <- HK_heatmap + scale_bar + north_arrow +
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

#Dotplot
ggmap(HK_map) +
  geom_polygon(data = WCZ, aes(x = lon, y = lat,  fill = wcz), colour = NA, size = 0.1, alpha = 0.6) +
  scale_fill_manual(values = brewer.pal(10, "Set3")) +
  new_scale_fill() + 
  geom_point(data = HK_locations, aes(x = lon, y = lat, colour = Group), size = 1) +
  #scale_fill_manual(values = c(brewer.pal(8, "Dark2")[1], brewer.pal(8, "Dark2")[2], brewer.pal(8, "Dark2")[8])) +
  scale_colour_manual(values = c("black", "white", "#FFD23C")) +
  coord_map() +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        legend.title = element_blank(), legend.position = "none")
