library(sf)
library(dplyr)
library(rmapshaper)
# source from analysis root
source(taf.boot.path("../utilities.R"))

library(icesTAF)

download.file(
  "https://sdi.eea.europa.eu/datashare/s/EjRWzWtxxAfGWGo/download?path=%2FGPKG&files=Regional_seas_around_Europe.gpkg&downloadStartSecret=bfcr6gbo4nf",
  destfile = "Regional_seas_around_Europe.gpkg",
  mode = "wb"
)

# read in data
msfd <- read_sf("Regional_seas_around_Europe.gpkg")

# filter only subregions
msfd <-
  st_zm(msfd)  %>%
  filter(
    !Id %in% c("MED", "ICE", "NOR", "WHI", "BLA", "ATL", "BAR")
  )

# simplify for quick plotting
msfd <-
  ms_simplify(
    msfd,
    keep = 0.001,
    keep_shapes = FALSE
  )

# transform to 4326
msfd <- st_transform(msfd, 4326)

# add subregion midpoints for circles if we want them
msfd_midpoint <- sf::st_coordinates(sf::st_centroid(msfd))

msfd$LON <- msfd_midpoint[,"X"]
msfd$LAT <- msfd_midpoint[,"Y"]

# save out
#st_write(msfd, "msfd.gpkg", append=FALSE)
st_write(msfd, "msfd.shp", append=FALSE)

# clean up
unlink("Regional_seas_around_Europe.gpkg")

if (FALSE) {
  # some useful bits
  msfd_data <- as.data.frame(msfd) %>% select(Id, name, spZoneType)
  sf::st_centroid(sf::st_union(msfd))
  plot_map(msfd,
    strokecolor = "#097FB3",
    fillcolor = "#AED3E4"
  )
}
