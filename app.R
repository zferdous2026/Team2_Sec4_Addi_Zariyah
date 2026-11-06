library(shiny)
library(dplyr)
library(readr)
library(leaflet)
library(stringr)
library(USpopcenters)

breast_cancer <- read_csv("IHME_USA_BREAST_CANCER_COUNTY_RACE_ETHN_2000_2019_MX_2019_BOTH_Y2026M03D25.csv", show_col_types = FALSE)

state_choices  <- breast_cancer %>% filter(str_detect(location_name, "\\(")) %>% 
  transmute(state = str_extract(location_name, "(?<=\\().*(?=\\))")) %>% pull(state) %>% unique() %>% sort()

county_choices <- breast_cancer %>% filter(str_detect(location_name, "\\(")) %>% 
  transmute(county = str_remove(location_name, " \\(.*\\)$")) %>% pull(county) %>% unique() %>% sort()

race_choices   <- unique(breast_cancer$race_ethnicity)

data("county2020", package = "USpopcenters")

# Helper regex to clean up words like "County", "Parish", etc.
clean_regex <- "(?i)\\s+(county|parish|borough|census area|municipality|city)$"

county_coordinates <- county2020 %>%
  transmute(
    join_key = paste(str_to_lower(str_remove(COUNAME, clean_regex)), str_to_lower(STNAME), sep = "-"),
    latitude = LATITUDE, longitude = LONGITUDE)

geocoded_data <- breast_cancer %>%
  distinct(location_name) %>%
  filter(str_detect(location_name, "\\(.*\\)$")) %>%
  mutate(
    co_clean = str_to_lower(str_remove(str_remove(location_name, " \\(.*\\)$"), clean_regex)),
    st_clean = str_to_lower(str_extract(location_name, "(?<=\\().*(?=\\))")),
    join_key = paste(co_clean, st_clean, sep = "-")
  ) %>%
  left_join(county_coordinates, by = "join_key", relationship = "many-to-many") %>%
  filter(!is.na(latitude))



ui <- navbarPage(
  title = "Breast Cancer Project", id = "tabs",
  header = tags$head(tags$style(HTML("
    body {background-color: #fff0f5;}
    h1, h2 {color: #c2185b; text-align: center;}
    h3, h4 {color: #ad1457; text-align: center;}
    p {color: #ad1457; text-align: center; font-size: 16px;}
    .map-container {display: flex; justify-content: center; margin: 20px;}
    .navbar {background-color: #c2185b;}
    .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a {color: white;}
  "))),
  
  tabPanel("Home",
           h1("Beyond the Numbers"),
           h3("Understanding Breast Cancer Mortality in the U.S."),
           p("Explore 2019 breast cancer mortality data across racial and ethnic groups and locations."),
           div(class = "map-container", leafletOutput("homepageMap", width = "80%", height = "500px")),
           h4("United States Breast Cancer Mortality Map"),
           p("Created by Zariyah Williams and Addi Johnson")
  ),
  
  tabPanel("Explore Data",
           h2("Explore Breast Cancer Mortality"),
           p("Filter breast cancer mortality data by location and race/ethnicity."),
           fluidRow(
             column(4, selectInput("state_filter", "Select State:", choices = c("All States", state_choices))),
             column(4, selectInput("county_filter", "Select County:", choices = c("All Counties", county_choices))),
             column(4, checkboxGroupInput("race_filter", "Select Racial/Ethnic Groups:", choices = race_choices, selected = race_choices))
           )
  )
)





server <- function(input, output, session) {
  output$homepageMap <- renderLeaflet({
    leaflet(geocoded_data, options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude, radius = 5,
        color = "#FF1493", fillColor = "#FF69B4", fillOpacity = 0.7, stroke = TRUE,
        popup = ~paste0("<b>County:</b> ", location_name), clusterOptions = markerClusterOptions()
      )
  })
}

shinyApp(ui = ui, server = server)