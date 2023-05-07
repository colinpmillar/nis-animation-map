#load R packages
library(shiny)
library(leaflet)
library(RColorBrewer)
library(xts)
library(rgdal)
library(dplyr)
library(lubridate)

source("utilities.R")

# helper function in JS for choropleth animation
leafletjs <- tags$head(
  tags$script(HTML('

window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  style = HTMLWidgets.dataframeToD3(style);
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.setStyle(style[i]);
    }
  });
};
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
'))
)



#load spatial data
msfd <- readOGR(
  dsn = "data/msfd",
  layer = "msfd",
  verbose = FALSE
)

#load nis data set
nisData <- read.csv("data/nis/nis_totals.csv")
nisData$Date_reported <- as.Date(nisData$date)

speciesList <- unique(nisData$Species)
names(speciesList) <- sapply(strsplit(speciesList, " "), function(x) paste(x[1:2], collapse = " "))

#select a certain date and species
selectedData <- nisData[nisData$Date_reported == min(nisData$Date_reported), ]
selectedData <- selectedData[selectedData$Species == selectedData$Species[1], ]

#match cases and spatial data via Id/Country Code
msfd$Cases <- selectedData$present[match(msfd$Id, selectedData$subregion.code)]

#create label texts
msfd@data$LabelText <- paste0(
  "<b>Subregion:</b> ", msfd@data$name,"<br>",
  "<b>Present:</b> ", format(msfd@data$Cases, nsmall=0, big.mark=","))

# define colorpalette for chart legend
paletteBins <- seq(1, 50, by = 5)
colorPalette <- colorBin(palette = "YlOrBr", domain = nisData$present, na.color = "transparent", bins = paletteBins)


#shiny UI
ui <- fluidPage(
  leafletjs,
  titlePanel("NIS Presence Development"),

  sidebarPanel(
    width = 2,

    selectInput(
      inputId = "species",
      label = "Species:",
      choices = speciesList,
      selected = 1
    ),

    uiOutput("dateUI")
  ),

  mainPanel(width = 10,
    leafletOutput("map", width = "70%", height = "750px")
  )
)


#shiny server
server <- function(input, output, session) {

  #create slider input depending on data frequency
  observe({

    allDates <- unique(nisData$Date_reported)
    eligibleDates <- allDates[xts::endpoints(allDates, on = "years")]

    stepSize <- 5

    output$dateUI <- renderUI({
      sliderInput("dateSel", "Date",
        min = min(eligibleDates),
        max = max(eligibleDates),
        value = min(eligibleDates),
        step = stepSize,
        timeFormat = "%y-%m-%d",
        animate = animationOptions(interval = 1, loop = TRUE)
      )
    })

    # define colorpalette for chart legend
    #paletteBins <- seq(1, max(nisData$present), by = 2)
    #colorPalette <- colorBin(palette = "YlOrBr", domain = nisData$present, na.color = "transparent", bins = paletteBins)

  })

  #filter data depending on selected date
  filteredData <- reactive({
    req(input$dateSel)
    nisData[nisData$Date_reported == make_date(year = year(input$dateSel)) & nisData$Species == input$species, ]
  })

  #create the base leaflet map
  output$map <- renderLeaflet({

    leaflet(msfd) %>%
      addProviderTiles(providers$Esri.WorldImagery,
        options = providerTileOptions(opacity = 0.5)
      ) %>%
      setView(lat = 50, lng = -6, zoom = 4) %>%

      addPolygons(
        layerId = ~Id,
        fillColor = "lightgray",
        stroke = TRUE,
        fillOpacity = 1,
        color = "white",
        weight = 1
      ) %>%

      #need to specify the leaflet::addLegend function here to avoid ambiguity with the xts::addLegend function
      leaflet::addLegend(pal = colorPalette, values = nisData$present, opacity = 0.9, title = "Years since present", position = "bottomleft")

  })


  #prepare data depending on selected date and draw either markers or update polygons depending on the selected map type
  observe({

    msfd$Cases <- filteredData()$present[match(msfd$Id, filteredData()$subregion.code)]

    msfd@data$LabelText <- paste0(
      "<b>Subregion:</b> ", msfd@data$name,"<br>",
      "<b>Present:</b> ", format(msfd@data$Cases, nsmall=0, big.mark=","))

    leafletProxy("map", data = msfd) %>%
        clearMarkers() %>%
        setShapeStyle(layerId = ~Id, fillColor = ~ifelse(Cases > 0, colorPalette(Cases), "lightgray"), label = msfd$LabelText)
  })
}


#shiny UI
ui <- fluidPage(
  leafletjs,
  titlePanel("NIS Presence Development"),

  sidebarPanel(
      width = 2,
      selectInput(
      inputId = "species",
      label = "Species:",
      choices = speciesList,
      selected = 1
    ),

    radioButtons(
      inputId = "frequency",
      label = "Select Data Frequency",
      choices = c("years"),
      selected = "years",
      inline = TRUE
    ),

    uiOutput("dateUI")
  ),

  mainPanel(width = 10,
    leafletOutput("map", width = "70%", height = "750px")
  )
)


#shiny server
server <- function(input, output, session) {

  #create slider input depending on data frequency
  observe({

    allDates <- unique(nisData$Date_reported[nisData$Species == input$species])
    eligibleDates <- allDates[xts::endpoints(allDates, on = "years")]

    stepSize <- 5

    output$dateUI <- renderUI({
      sliderInput("dateSel", "Date",
        min = min(eligibleDates),
        max = max(eligibleDates),
        value = min(eligibleDates),
        step = stepSize,
        timeFormat = "%Y",
        animate = animationOptions(interval = 1, loop = FALSE)
      )
    })

  })

  #filter data depending on selected date
  filteredData <- reactive({
    req(input$dateSel)
    nisData[nisData$Date_reported == make_date(year = year(input$dateSel)) & nisData$Species == input$species, ]
  })

  #create the base leaflet map
  output$map <- renderLeaflet({

    # define colorpalette for chart legend
    paletteBins <- seq(1, max(nisData$present[nisData$Species == input$species], na.rm = TRUE), by = 2)
    colorPalette <- colorBin(palette = "YlOrBr", domain = nisData$present[nisData$Species == input$species], na.color = "transparent", bins = paletteBins)

    leaflet(msfd) %>%
      addTiles()  %>%
      setView(lat = 50, lng = -6, zoom = 4) %>%

      addPolygons(
        layerId = ~Id,
        fillColor = "lightgray",
        stroke = TRUE,
        fillOpacity = 1,
        color = "white",
        weight = 1
      ) %>%

      #need to specify the leaflet::addLegend function here to avoid ambiguity with the xts::addLegend function
      leaflet::addLegend(pal = colorPalette, values = nisData$present[nisData$Species == input$species], opacity = 0.9, title = "Years present", position = "bottomleft")

  })


  #prepare data depending on selected date and draw either markers or update polygons depending on the selected map type
  observe({

    msfd$Cases <- filteredData()$present[match(msfd$Id, filteredData()$subregion.code)]

    msfd@data$LabelText <- paste0(
      "<b>Subregion:</b> ", msfd@data$name,"<br>",
      "<b>Present:</b> ", format(msfd@data$Cases, nsmall=0, big.mark=","))

    leafletProxy("map", data = msfd) %>%
        clearMarkers() %>%
        setShapeStyle(layerId = ~Id, fillColor = ~ifelse(Cases > 0, colorPalette(Cases), "lightgray"), label = msfd$LabelText)
  })
}

shinyApp(ui, server)
