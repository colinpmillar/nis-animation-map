library(sf)
library(dplyr)
library(lubridate)
library(icesTAF)

mkdir("data")

if (FALSE) {
  # download data
  download.file(
    "https://sdi.eea.europa.eu/datashare/s/H3xGXoMm4WC55ji/download?path=%2F&files=NIS-5-yr-interval.xls&downloadStartSecret=fzom2137zbf",
    destfile = "NIS-5-yr-interval.xls",
    mode = "wb"
  )

  # read in data - hold for now - need to add subregion and bits
  newnis <- readxl::read_excel("NIS-5-yr-interval.xls", sheet = 1)
}

nis <-
  read.csv(taf.data.path("Data_package_MAR002_NIS_Dataset.csv"))

nis_keep <-
  nis %>%
  filter(STATUS %in% c("non-indigenous", "non-indigenous?", "genus level"), Period != "<1970") %>%
  select(Species, Region, Region.code, Subregion, subregion.code, Group, Year, Period, REL, EC, TS.0ther, TS.ball, TS.hull, COR, UNA, UNK)

nis_keep$Year[nis_keep$Year == "2016-2017"] <- "2017"
nis_keep$Year[nis_keep$Year == "<1978"] <- "1977"
nis_keep$Year <- as.numeric(nis_keep$Year)

# nis_test <- nis_keep %>% filter(Species == grep("Mnemiopsis", nis_keep$Species, value = TRUE))
species_list <- table(nis_keep$Species)
nis_test <- nis_keep %>% filter(Species %in% names(species_list[species_list > 5]))

# nis_test %>% arrange(Year, subregion.code)

nisData <-
  nis_test %>%
  tibble() %>%
  select(Year, Subregion, subregion.code, Species) %>%
  mutate(
    date = make_date(year = as.numeric(Year)),
    present = 1
  )


fill <-
  by(
    nisData, nisData$Species,
    function(x) {
      out <-
        expand.grid(
          Year = seq(min(x$Year), max(x$Year)),
          subregion.code = unique(x$subregion.code),
          Species = unique(x$Species)
        ) %>%
        left_join(unique(x[c("Subregion", "subregion.code", "Species")]))
      rownames(out) <- NULL
      out
    }
  )
fill <- do.call(rbind, fill)
rownames(fill) <- NULL

nis_totals <-
  nisData %>%
  right_join(fill) %>%
  mutate(
    date = make_date(year = as.numeric(Year))
  ) %>%
  arrange(Year) %>%
    group_by(Subregion, subregion.code, Species) %>%
    mutate(
      present = cumsumNA(present)
    ) %>%
    arrange(Species, subregion.code, Year)




write.taf(nis_totals, dir = "data", quote = TRUE)
