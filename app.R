#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://posit.co
#

library(shiny)
library(bslib)
library(dplyr)
library(readr) 
library(ggplot2)
library(ggExtra)
library(leaflet)
library(tidygeocoder)


breast_cancer <- read_csv("IHME_USA_BREAST_CANCER_COUNTY_RACE_ETHN_2000_2019_MX_2019_BOTH_Y2026M03D25.csv")

geocoded_data <- breast_cancer %>%
  geocode(address = location_name, method = 'osm', lat = latitude, long = longitude)

ui <- navbarPage(
  title = "Breast Cancer Project",
  id = "tabs",
  
  header = tags$head(
    tags$style(HTML("
      body { background-color: #fff0f5; }
      h1 { color: #c2185b; text-align: center; }
      h3 { color: #ad1457; text-align: center; }
      p { text-align: center; font-size: 16px; }
    "))
  ),
  
  tabPanel("Home",
           h1("Beyond the Numbers"),
           h3("Understanding Breast Cancer Mortality in the U.S."),
           p("Explore 2019 breast cancer mortality data across racial and ethnic groups and locations."),
           h4("Breast Cancer Mortality Visually", style = "text-align: center; color: #ad1457;"),
           p("Created by Zariyah Williams and Addi Johnson")),
  
  tabPanel("Explore Data", 
           h2("Explore the Data")))

server <- function(input, output, session) { output$homepageMap <- renderLeaflet({
  leaflet(geocoded_data) %>% 
    addTiles() %>% 
    setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>% 
    addCircleMarkers(
      lng = ~longitude,  # Matches the column name created by geocode()
      lat = ~latitude,   # Matches the column name created by geocode()
      radius = 6, 
      color = "#FF1493", 
      fillColor = "#FF69B4", 
      fillOpacity = 0.7, 
      stroke = TRUE, 
      popup = ~paste0("<b>County: </b>", location_name)
    )})}

shinyApp(ui = ui, server = server)
