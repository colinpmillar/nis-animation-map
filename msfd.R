library(sf)
library(dplyr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(rmapshaper)

source("plot_map.R")

# read in data
msfd <- read_sf("Regional_seas_around_Europe.gpkg")


msfd <- 
  st_zm(msfd)  %>%
  filter(
    !Id %in% c("MED", "ICE", "NOR", "WHI", "BLA", "ATL", "BAR") 
  )


msfd <- ms_simplify(msfd, keep = 0.001,
                                keep_shapes = FALSE)

msfd_midpoint <- sf::st_coordinates(sf::st_centroid(msfd))

msfd$LON <- msfd_midpoint[,"X"]
msfd$LAT <- msfd_midpoint[,"Y"]


plot_map(msfd, strokecolor = '#097FB3',
         fillcolor = '#AED3E4')


msfd <- st_transform(msfd, 4326)


st_write(msfd, "msfd.gpkg", append=FALSE)
st_write(msfd, "msfd.shp", append=FALSE)

msfd_data <- as.data.frame(msfd) %>% select(Id, name, spZoneType)


sf::st_centroid(sf::st_union(msfd))


msfd_midpoint <- 
  sf::st_centroid(msfd) %>%
  select(Id, name)
plot(msfd_midpoint[2])

