#' Shiny UI for the Application
#'
#' This function creates the user interface (UI) for the Shiny application, allowing users to search for a location and displaying its latitude and longitude on a map.
#'
#' @title Shiny UI for the Application
#' @description This UI includes a text input box for entering the location, a search button, and a map display area.
#' @export
#' @import shiny
#' @import magrittr
#' @import leaflet
#' @import rvest
#' @import knitr
#' @import tidyverse
#' @import dplyr
#' @import httr
#' @import jsonlite
#' @name ui
ui <- fluidPage(
  # ... UI code here ...
)

#' Shiny Server Function
#'
#' This function defines the server logic for the Shiny application, including handling user inputs, retrieving latitude and longitude, and displaying markers on the map.
#'
#' @param input Shiny input values
#' @param output Shiny output values
#' @param session Shiny session object
#' @export
#' @importFrom dplyr %>%
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel textInput actionButton tags$style div leafletOutput makeAwesomeIcon renderLeaflet observeEvent
#' @name server


library(shiny)
library(magrittr)
library(leaflet)
library(rvest)
library(knitr)
library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)


# User interface for the application that generates a map
ui <- fluidPage(
  
  # shinyApp Title
  titlePanel("Looking for the place?"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      h3(textInput(inputId = "begin",label = "Enter here")),
      actionButton(inputId = "Find",label = "Search")
    ),
    
    # The map along with the geographical coordinates.
    mainPanel(
      tags$style("#Location {font-size:16px;
               color:darkblack;
                 display:block; }"),     
      div(style="text-align:center;
        box-shadow: 4px 4px 5px #888888;
          width:250px;
          height:200px;
          padding-top:100px;
          position:relative;",
          textOutput(outputId = "Location")),
      div(style="text-align:center;
          width:800px;
          height:800px;
          padding-top:100px;
          position:relative;",
          leafletOutput(outputId = "mymap"))
    )
  )
)

# Specifying the server logic to be displayed on the map.
server <- function(input, output) {
  reactivedata = reactive({
    return(lat_long(input$begin))
  })
  
  
  observeEvent(input$Find,{
    
    
    
    x1=reactivedata()
    
    lat <- as.numeric(unname(x1[[1]]))
    lng <- as.numeric(unname(x1[[2]]))
    
    o1<-paste("Latitude:",lat,sep="")
    o2<-paste("Longitude:",lng,sep="")
    
    output$Location <- renderText(paste(c(o1,o2)))
    
    icon.fa <- makeAwesomeIcon(
      icon = "flag", markerColor = "red",
      library = "fa",
      iconColor = "black"
    )
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(
          "OpenStreetMap",
          # give the layer a name
          group = "OpenStreetMap"
        ) %>%
        addAwesomeMarkers(
          lat = lat,
          lng = lng,
          label = "Here",
          icon = icon.fa
        )
      
      
    })
  })
}

# Run the shinyApp
shinyApp(ui = ui, server = server)

