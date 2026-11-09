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

race_choices   <- unique(breast_cancer$race_name)

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
    body {background-color: white;}
    h1, h2 {color: #c2185b; text-align: center;}
    h4 {color: #ad1457; text-align: center;}
    p {color: #ad1457; text-align: center; font-size: 16px;}
    .map-container {display: flex; justify-content: center; margin: 20px;}
    .navbar {background-color: #c2185b;}
    .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a {color: white;}
  "))),
  
  tabPanel("Home",
           h1("Understanding Breast Cancer Mortality in the U.S."),
           
           p("Breast cancer is a disease that affects roughly 1 in 8 women and around 3,000 cases for men each year in the United States and is the most common cancer among women (American Cancer Society). From this, it is estimated 42,140 women die from breast cancer, so this is a disease that affects millions of United States citizens and their families. Each year, about 32% of all newly diagnosed cancers in women are breast cancer and with this black women have 5% lower rate of breast cancer diagnosis compared to white women, but they have a 38% higher risk of dying. The median age at the time of breast cancer diagnosis is 62, but rates have been rising in younger women in recent years (BreastCancer.Org)."),
           
           div(class = "map-container", leafletOutput("homepageMap", width = "80%", height = "500px")),
           p("United States Breast Cancer Mortality Map"),
           
           h4("Risk Factors"),
           p("There are many risk factors for breast cancer, but a few include sex at birth and getting older, family history, and genetics. The first, and most significant, risk factor is being an aging woman because as women get older, breast cancer rates increase along with the survival rate decreasing. Secondly, if you have a family history of breast cancer then your risk nearly doubles if that relative is a first-degree relative. Approximately 15% of women who get breast cancer have a family member diagnosed with it. Finally, genetic factors play a huge part when it comes to risk factors for breast cancer due to the BRCA1 and BRCA2 genes, the most commonly mutated genes. About 5% to 10% of breast cancers can be linked to known gene mutations inherited from one’s mother or father. However, about 85% of breast cancers occur in women who have no family history of breast cancer due to mutations in these genes that happen as a result of the general aging process (BreastCancer.Org) "),
           
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
  ),
  
  tabPanel("About Us",
           h2("Addi adds stuff about self"),
           h2("Zariyah adds stuff about self")
           ),

  tabPanel("References",
           p("paste references"))
  
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