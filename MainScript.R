library(sf)
library(tidyverse)
library(osmdata)
library(osmextract)
library(patchwork) # for placing plots next to each other for comparison

sf::sf_use_s2(FALSE)

# Read in station data
dc_stations <- st_read("data/Metro_Stations_in_DC.geojson")

roads <- st_transform(oe_get(
  "Washington, DC",
  quiet = FALSE,
  query = "SELECT highway, geometry FROM 'lines'"
), 4269)

filtered_roads <- roads |> 
  dplyr::filter(highway %in% c(
    "motorway", "trunk", "primary", "secondary", "tertiary", "service",
    "residential", "unclassified","footway", "path", "cycleway"
  ))

buildings <- st_transform(oe_get(
  "Washington, DC",
  quiet = FALSE,
  layer = "multipolygons",
  query = "SELECT * FROM multipolygons WHERE building IS NOT NULL"
), 4269)

# below was a test

# single_station <- dc_stations_nad |> 
#   dplyr::filter(NAME == "L'Enfant Plaza")
# 
# single_station_buffer <- st_buffer(single_station, 402)
# 
# clipped_station_roads <- st_intersection(filtered_roads, single_station_buffer)
# raw_station_roads <- st_intersection(roads, single_station_buffer)
# clipped_station_buildings <- st_intersection(buildings, single_station_buffer)
# 
# filtered_roads_plot <- ggplot(clipped_station_roads) + geom_sf()
# raw_roads_plot <- ggplot(raw_station_roads) + geom_sf()
# buildings_plot <- ggplot(clipped_station_buildings) + geom_sf()
# filtered_roads_plot + raw_roads_plot / buildings_plot

# convert station data to NAD83
dc_stations_nad83md <- st_transform(dc_stations, 2248)
# convert roads to nad83
roads_nad83md <- st_transform(filtered_roads, 2248)
# convert buildings to nad83
buildings_nad83md <- st_transform(buildings, 2248)

clip_to_station <- function(roads, buildings, metro_stops, station) {
  roads_buffered <- st_buffer(roads, 10)
  current_station <- metro_stops |> 
    dplyr::filter(NAME == station)
  one_station_buffer = st_buffer(current_station, 1320)
  clipped_roads_buffer = st_intersection(roads_buffered, one_station_buffer)
  clipped_buildings = st_intersection(buildings, one_station_buffer)
  ggplot() + 
    geom_sf(data = clipped_buildings, fill = 'blue') +
    geom_sf(data = clipped_roads_buffer, fill = 'purple')
  ggsave(
    filename = paste0("station_maps/", station, ".png")
  )
}

clip_to_station(roads_nad83md, buildings_nad83md, dc_stations_nad83md, "L'Enfant Plaza")

for (x in 1:nrow(dc_stations_nad83md)) {
  current_station <- dc_stations_nad83md[x, ]
  clip_to_station(roads_nad83md, buildings_nad83md, dc_stations_nad83md, current_station$NAME)
}

ggplot() + 
  geom_sf(data = clipped_station_buildings, aes(fill = 'gray')) +
  geom_sf(data = clipped_station_roads)

# things to do:
# EXPORT ROADS AND BUILDINGS FILES SO I DON'T HAVE TO QUERY THEM EVERYTIME.
# IDENTIFY THE BEST COMBINATION OF ROADS AND PATHS TO KEEP
# CREATE BUFFER AND GENERATE SAMPLE IMAGERY WITH COLOR CODED ROADS POTENTIALLY
# IF ABOVE DOESN'T LOOK GOOD SEE ABOUT A QUICK WAY TO DO THE LAND USE THINK LIKE POUSSON SENT ON DISCORD (DM)