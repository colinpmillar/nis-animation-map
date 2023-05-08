library(sf)
library(dplyr)
library(lubridate)

# read in data


nis <- read.csv("NIS_msfd_coord.csv", encoding= "UTF-8") %>%
  filter(STATUS == "non-indigenous", Year >= 1970)

nis$Year[nis$Year == "2016-2017"] <- "2016"

# calculate totals shapefile

nis_totals <-
  nis %>%
  tibble() %>%
  select(Year, subregion.code, Species, STATUS) %>%
  group_by(Year, subregion.code) %>%
  count() %>%
  rename(nis = n) %>% 
  mutate(
    date = make_date(year = as.numeric(Year))
  ) %>%
  left_join(msfd_midpoint, by = c("subregion.code" = "Id"))

nis_totals

st_write(nis_totals, "nis_totals.gpkg", append=FALSE)



