## Collect data, web files, and create the shiny app

## Before:
## After:

library(icesTAF)
library(rmarkdown)

mkdir("shiny")

# create shiny app data folder
mkdir("shiny/data")
mkdir("shiny/data/nis")

# copy in required data
cp("data/nis_totals.csv", "shiny/data/nis/nis_totals.csv")
cp("boot/data/msfd", "shiny/data/")


# copy in utilities
cp("utilities.R", "shiny")


# copy in server and ui scripts
cp("shiny_app.R", "shiny/app.R")

msg("Created shiny app. To run, use: \n\n\tshiny::runApp('shiny')\n\n")
