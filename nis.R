library(sf)
library(dplyr)
library(lubridate)

# read in data

nis <- read.csv("Data_package_MAR002_NIS_Dataset.csv", encoding= "UTF-8") 

nis_keep <-
  nis %>%
  filter(STATUS %in% c("non-indigenous","non-indigenous?", "genus level"), Period != "<1970") %>%
  select(Species, Region, Region.code, Subregion, subregion.code, Group, Year, Period, REL, EC, TS.0ther, TS.ball, TS.hull, COR, UNA, UNK)

nis_keep$Year[nis_keep$Year == "2016-2017"] <- "2017"
nis_keep$Year[nis_keep$Year == "<1978"] <- "1977"
nis_keep$Year <- as.numeric(nis_keep$Year)


sps <- sort(table(nis_keep$Species))
sps[sps > 5]


nis_test <- nis_keep %>% filter(Species == grep("Mnemiopsis", nis_keep$Species, value = TRUE))
nis_test <- nis_keep %>% filter(Species %in% names(sps[sps > 5]))



nis_test %>% arrange(Year, subregion.code)
# 



nis_totals <-
  nis_test %>%
  tibble() %>%
  select(Year, Subregion, subregion.code, Species) %>%
  mutate(
    date = make_date(year = as.numeric(Year)),
    present = 1
  )


#  left_join(msfd_midpoint, by = c("subregion.code" = "Id"))
write.csv(nis_totals, file = "nis_totals.csv", row.names = FALSE)
