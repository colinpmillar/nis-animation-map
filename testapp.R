#load R packages
library(shiny)
library(leaflet)
library(RColorBrewer)
library(xts)
library(rgdal)

source("utilities.R")


#load spatial data
msfd <- readOGR( 
  dsn = getwd() , 
  layer = "msfd",
  verbose = FALSE
)

#load nis data set
nisData <- read.csv("nis_totals.csv")

fill <- 
  expand.grid(
    Year = seq(min(nisData$Year), max(nisData$Year)),
    subregion.code = unique(nisData$subregion.code),
    Species = unique(nisData$Species)
  ) %>%
  left_join(unique(nisData[c("Subregion", "subregion.code", "Species")]))

nisData <- 
  nisData %>%
  right_join(fill) %>%
  mutate(
    date = make_date(year = as.numeric(Year))
  ) %>%
  arrange(Year) %>%
  group_by(Subregion, subregion.code, Species) %>%
  mutate(
    present = cumsumNA(present)
  )

nisData$Date_reported <- as.Date(nisData$date)

speciesList <- unique(nisData$Species)
names(speciesList) <- speciesList

  


#select a certain date
selectedData <- nisData[nisData$Date_reported == min(nisData$Date_reported), ]
selectedData <- selectedData[selectedData$Species == selectedData$Species[1], ]


#match cases and spatial data via Id/Country Code
msfd$Cases <- selectedData$present[match(msfd$Id, selectedData$subregion.code)]

#create label texts
msfd@data$LabelText <- paste0(
  "<b>Subregion:</b> ", msfd@data$name,"<br>", 
  "<b>Present:</b> ", format(msfd@data$Cases, nsmall=0, big.mark=","))

#define colorpalette for chart legend
paletteBins <- seq(1, 50, by = 5)
colorPalette <- colorBin(palette = "YlOrBr", domain = nisData$present, na.color = "transparent", bins = paletteBins)

#shiny UI
ui <- fluidPage(
  leafletjs,
  titlePanel("NIS Presence Development"),
  
  sidebarPanel(width = 2,
               
               radioButtons(inputId = "mapType",
                            label = "Select Map Type",
                            choices = c("Markers", "Choropleth"),
                            selected = "Choropleth",
                            inline = TRUE),
               
               selectInput(inputId = "species", 
                           label = "Species:",
                           choices = speciesList,
                           selected = 1),
               
               
               radioButtons(inputId = "frequency",
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
    
    allDates <- unique(nisData$Date_reported)
    eligibleDates <- allDates[xts::endpoints(allDates, on = input$frequency)]
    
    stepSize <- 5
    
    output$dateUI <- renderUI({
      sliderInput("dateSel", "Date",
                  min = min(eligibleDates),
                  max = max(eligibleDates),
                  value = min(eligibleDates),
                  step = stepSize,
                  timeFormat = "%y-%m-%d",
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
      leaflet::addLegend(pal = colorPalette, values = nisData$present, opacity = 0.9, title = "Years since present", position = "bottomleft")
    
  })
  
  
  #prepare data depending on selected date and draw either markers or update polygons depending on the selected map type
  observe({
    
    msfd$Cases <- filteredData()$present[match(msfd$Id, filteredData()$subregion.code)]
    
    msfd@data$LabelText <- paste0(
      "<b>Subregion:</b> ", msfd@data$name,"<br>", 
      "<b>Present:</b> ", format(msfd@data$Cases, nsmall=0, big.mark=","))
    
    if(input$mapType == "Markers"){
      
      leafletProxy("map", data = msfd) %>%
        clearMarkers() %>%
        setShapeStyle(layerId = ~Id, fillColor = "lightgray") %>%
        addCircleMarkers(lng = ~LON,
                         lat = ~LAT,
                         radius = ~Cases * 2,
                         weight = 1,
                         opacity = 1,
                         color = ~ifelse(Cases > 0, "black", "transparent"),
                         fillColor = ~ifelse(Cases > 0, colorPalette(Cases), "transparent"),
                         fillOpacity = 0.8,
                         label = ~lapply(LabelText, htmltools::HTML))
      
    }else if(input$mapType == "Choropleth"){
      
      leafletProxy("map", data = msfd) %>%
        clearMarkers() %>%
        setShapeStyle(layerId = ~Id, fillColor = ~ifelse(Cases > 0, colorPalette(Cases), "lightgray"), label = msfd$LabelText)
      
    }
  })
}

shinyApp(ui, server) 