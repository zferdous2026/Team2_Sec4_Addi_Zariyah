library(shiny)
library(dplyr)
library(readr)
library(leaflet)
library(stringr)
library(USpopcenters)
library(ggplot2)

breast_cancer <- read_csv(
  "IHME_USA_BREAST_CANCER_COUNTY_RACE_ETHN_2000_2019_MX_2019_BOTH_Y2026M03D25.csv",
  show_col_types = FALSE
)

# CLEAN UP
if ("metric_name" %in% names(breast_cancer) &&
    "Rate" %in% breast_cancer$metric_name) {
  breast_cancer <- breast_cancer %>%
    filter(metric_name == "Rate")
}

if ("age_name" %in% names(breast_cancer) &&
    "Age-standardized" %in% breast_cancer$age_name) {
  breast_cancer <- breast_cancer %>%
    filter(age_name == "Age-standardized")
}

# MORE CLEAN UP
county_data <- breast_cancer %>%
  filter(str_detect(location_name, "\\(.*\\)$")) %>%
  mutate(
    state = str_extract(location_name, "(?<=\\().*(?=\\))"),
    county = str_remove(location_name, " \\(.*\\)$")
  )

state_choices <- sort(unique(county_data$state))
county_choices <- sort(unique(county_data$county))
race_choices <- sort(unique(breast_cancer$race_name))

# sigh more clean up
data("county2020", package = "USpopcenters")

clean_regex <- "(?i)\\s+(county|parish|borough|census area|municipality|city)$"

county_coordinates <- county2020 %>%
  transmute(
    join_key = paste(
      str_to_lower(str_remove(COUNAME, clean_regex)),
      str_to_lower(STNAME),
      sep = "-"
    ),
    latitude = LATITUDE,
    longitude = LONGITUDE
  )

geocoded_data <- county_data %>%
  distinct(location_name, county, state) %>%
  mutate(
    co_clean = str_to_lower(str_remove(county, clean_regex)),
    st_clean = str_to_lower(state),
    join_key = paste(co_clean, st_clean, sep = "-")
  ) %>%
  left_join(county_coordinates, by = "join_key") %>%
  filter(!is.na(latitude))



ui <- navbarPage(
  
  title = "Breast Cancer Project",
  id = "tabs",
  
  header = tags$head(tags$style(HTML("
    body {
      background-color: #fff8fb;
    }
    
    h1, h2, h3, h4 {
      color: #ad1457;
      text-align: center;
    }
    
    p {
      color: #ad1457;
      text-align: center;
      font-size: 16px;
      line-height: 1.6;
    }
    
    .navbar {
      background-color: #c2185b;
    }
    
    .navbar-default .navbar-brand,
    .navbar-default .navbar-nav > li > a {
      color: white;
    }
    
    .stat-card {
      background-color: #fce4ec;
      border-radius: 12px;
      padding: 20px;
      margin: 10px;
      min-height: 145px;
      text-align: center;
      box-shadow: 0 3px 8px rgba(0,0,0,0.08);
    }
    
    .stat-card h2 {
      font-size: 30px;
      font-weight: bold;
    }
    
    .info-card {
      background-color: white;
      border: 1px solid #f3c1d5;
      border-radius: 12px;
      padding: 20px;
      margin: 10px;
      min-height: 180px;
    }
    
    .map-container {
      margin: 20px auto;
      width: 95%;
      border-radius: 12px;
      overflow: hidden;
    }
    
    .section-card {
      background-color: white;
      border-radius: 12px;
      padding: 20px;
      margin: 15px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.06);
    }
  "))),
  
  
  # HOME
  tabPanel("Home",
           
           h1("Understanding Breast Cancer Mortality"),
           
           p("Exploring differences in breast cancer mortality across the United States by county, race, and ethnicity."),
           br(),
           
           
           fluidRow(
             column(4,
                    div(class = "stat-card",
                        h2("1 in 8"),
                        p("Women in the United States will develop breast cancer during their lifetime.")
                    )
             ),
             
             column(4,
                    div(class = "stat-card",
                        h2("38%"),
                        p("Higher risk of dying from breast cancer among Black women compared to white women.")
                    )
             ),
             
             column(4,
                    div(class = "stat-card",
                        h2("2019"),
                        p("Year covered by our breast cancer mortality dataset.")
                    )
             ),
             br(),
             br()
           ),
           
           br(),
           br(),
           
           h3("Breast Cancer Across the United States"),
           
           p("Explore the geographic locations of counties included in our dataset. Select a marker to see its county."),
           
           div(class = "map-container",
               leafletOutput("homepageMap", width = "100%", height = "500px")
           ),
           
           h3("Why Does This Data Matter?"),
           
           p("Breast cancer affects millions of people and their families. However, the impact of the disease is not the same across all racial and ethnic groups. Black women are diagnosed at slightly lower rates than white women but face a significantly higher risk of dying from the disease. Looking at mortality data by location and race can help reveal differences in health outcomes and highlight communities that may need greater attention and resources."),
           
           fluidRow(
             column(4,
                    div(class = "info-card",
                        h4("Age and Sex"),
                        p("Breast cancer risk generally increases with age, and women are significantly more likely to develop the disease than men.")
                    )
             ),
             
             column(4,
                    div(class = "info-card",
                        h4("Family History"),
                        p("Having a close relative with breast cancer can increase an individual's risk.")
                    )
             ),
             
             column(4,
                    div(class = "info-card",
                        h4("Genetics"),
                        p("Inherited mutations in genes such as BRCA1 and BRCA2 can increase the likelihood of developing breast cancer.")
                    )
             )
           ),
           
           h3("About This Project"),
           
           p("This project uses data from the Institute for Health Metrics and Evaluation (IHME) to examine breast cancer mortality across U.S. counties, racial groups, and ethnicities. Our goal is to make this information easier to explore and understand."),
           
           p("Created by Zariyah Williams and Addi Johnson")
  ),
  
  # NOT DONE
  tabPanel("Filter Data",
           
           h2("Explore Breast Cancer Mortality"),
           
           p("Explore differences in breast cancer mortality across U.S. counties and racial and ethnic groups. Use the filters below to compare populations and locations."),
           
           br(),
           
           fluidRow(
             column(4,
                    selectInput(
                      "state_filter",
                      "Select State:",
                      choices = c("All States", state_choices)
                    )
             ),
             
             column(4,
                    selectInput(
                      "county_filter",
                      "Select County:",
                      choices = c("All Counties", county_choices)
                    )
             ),
             
             column(4,
                    selectInput(
                      "race_filter",
                      "Select Racial/Ethnic Groups:",
                      choices = race_choices,
                      selected = race_choices,
                      multiple = TRUE
                    )
             ))
           ,
           
           br(),
           
           div(class = "section-card",
               h3("Mortality Rate Comparison"),
               p("Compare average breast cancer mortality rates across the selected racial and ethnic groups."),
               plotOutput("mortalityPlot", height = "450px")
           ),
           
           fluidRow(
             column(6,
                    div(class = "section-card",
                        h3("Mortality Rates by Race"),
                        p("View the average mortality rate for each selected racial and ethnic group."),
                        tableOutput("mortalityTable")
                    )
             ),
             column(6,
                    div(class = "section-card",
                        h3("Mortality Rate by State"),
                        p("View the average mortality rate for each state."),
                        div(style = "max-height: 400px; overflow-y: auto;",
                            tableOutput("stateTable"))
                    )
             ),
             column(6,
                    div(class = "section-card",
                        h3("Top 10 Highest-Rate Counties"),
                        p("Counties with the highest average mortality rates for the selected groups."),
                        plotOutput("lollipopPlot", height = "450px")
                    )
             ),
             column(6,
                    div(class = "section-card",
                        h3("Racial Disparity Gap"),
                        p("Difference between the lowest and highest group mortality rates by state or county."),
                        plotOutput("dumbbellPlot", height = "450px")
                    )
             )
           )),
  

  # NOT DONE
  tabPanel("Data Analysis",
           "blah blah"),
  
  
  # NOT DONE
  tabPanel("Survey",
           "blah blah"),
  
  
  # ABOUT US
  tabPanel("About Us",
           
           h2("About Us"),
           
           fluidRow(
             column(6,
                    div(class = "info-card",
                        h3("Zariyah Williams"),
                        p("Add stuff")
                    )
             ),
             
             column(6,
                    div(class = "info-card",
                        h3("Addi Johnson"),
                        p("Add stuff")
                    )
             )
           )),
  
  
  # REFERENCES
  tabPanel("References",
           
           h2("References"),
           
           div(class = "section-card",
               p("add sources")
           )
  )
)



server <- function(input, output, session) {
  
  
  output$homepageMap <- renderLeaflet({
    
    leaflet(geocoded_data) %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = "#FF1493",
        fillColor = "#FF69B4",
        fillOpacity = 0.7,
        popup = ~paste0("<b>County:</b> ", location_name),
        clusterOptions = markerClusterOptions()
      )
  })
  
  
  # CLEAN UP FOR FILTER
  observeEvent(input$state_filter, {
    
    if (input$state_filter == "All States") {
      choices <- county_choices
    } else {
      choices <- county_data %>%
        filter(state == input$state_filter) %>%
        pull(county) %>%
        unique() %>%
        sort()
    }
    
    updateSelectInput(
      session,
      "county_filter",
      choices = c("All Counties", choices),
      selected = "All Counties"
    )
  })
  
  
  filtered_data <- reactive({
    
    data <- county_data
    
    if (input$state_filter != "All States") {
      data <- data %>%
        filter(state == input$state_filter)
    }
    
    if (input$county_filter != "All Counties") {
      data <- data %>%
        filter(county == input$county_filter)
    }
    
    data <- data %>%
      filter(race_name %in% input$race_filter)
    
    data
  })
  
  
  # NOT DONE
  output$mortalityPlot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, "No data available. Try changing your filters.")
    )
    
    plot_data <- data %>%
      group_by(race_name) %>%
      summarise(
        rate = mean(val, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(plot_data, aes(x = race_name, y = rate, fill = race_name, cex = 4)) +
      geom_col(width = 0.7, fill="#ad1457") +
      labs(
        x = "Race/Ethnicity",
        y = "Average Mortality Rate"
      ) + 
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1, size=15),
          axis.text.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15)
      )
  })
  output$lollipopPlot <- renderPlot({
    df <- filtered_data() %>%
      group_by(location_name) %>%
      summarize(avg_rate = mean(val, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(avg_rate)) %>%
      slice_head(n = 10)
    
    validate(need(nrow(df) > 0, "No data available for the selected filters."))
    
    ggplot(df, aes(x = avg_rate, y = reorder(location_name, avg_rate))) +
      geom_segment(
        aes(x = 0, xend = avg_rate, yend = location_name),
        color = "#f48fb1", linewidth = 1.3
      ) +
      geom_point(color = "#ad1457", size = 4.5) +
      geom_text(
        aes(label = round(avg_rate, 1)),
        hjust = -0.35, color = "#880e4f", fontface = "bold", size = 3.8
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        x = "Mortality Rate (per 100,000)",
        y = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "#4a0e2e")
      )
  })
  
  # CHART 4: Racial Disparity Gap (Dumbbell Plot)
  output$dumbbellPlot <- renderPlot({
    # Group by State if viewing All States, otherwise group by County
    group_col <- if (input$state_filter == "All States") "state" else "county"
    
    df <- filtered_data() %>%
      group_by(location = .data[[group_col]], race_name) %>%
      summarize(rate = mean(val, na.rm = TRUE), .groups = "drop")
    
    # Calculate the min-to-max disparity gap per location and take top 12 gaps
    gap_summary <- df %>%
      group_by(location) %>%
      filter(n_distinct(race_name) >= 2) %>%
      summarize(
        min_rate = min(rate, na.rm = TRUE),
        max_rate = max(rate, na.rm = TRUE),
        gap = max_rate - min_rate,
        .groups = "drop"
      ) %>%
      arrange(desc(gap)) %>%
      slice_head(n = 12)
    
    validate(need(
      nrow(gap_summary) > 0,
      "Select at least 2 racial/ethnic groups across multiple locations to view disparity gaps."
    ))
    
    plot_df <- df %>%
      inner_join(gap_summary, by = "location")
    
    ggplot(plot_df, aes(x = rate, y = reorder(location, gap))) +
      geom_segment(
        data = gap_summary,
        aes(x = min_rate, xend = max_rate, y = location, yend = location),
        color = "#f8bbd0", linewidth = 2
      ) +
      geom_point(aes(color = race_name), size = 4) +
      scale_color_brewer(palette = "Dark2") +
      labs(
        x = "Mortality Rate (per 100,000)",
        y = NULL,
        color = "Group"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(color = "#4a0e2e")
      )
  })
  
  
  # done
  output$mortalityTable <- renderTable({
    data <- filtered_data()
    
    validate(
      need(!is.null(data) && nrow(data) > 0,
           "No data available for the selected groups.")
    )
    
    data %>%
      filter(metric_name == "Rate") %>%
      group_by(`Race/Ethnicity` = race_name) %>%
      summarise(`Average Mortality Rate (per 100,000)` = mean(val, na.rm = TRUE) * 100000,
                .groups = "drop") %>%
      arrange(desc(`Average Mortality Rate (per 100,000)`))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, digits = 2)
  
  output$stateTable <- renderTable({
    data <- filtered_data()
    
    validate(
      need(!is.null(data) && nrow(data) > 0,
           "No data available for the selected groups.")
    )
    
    data %>%
      filter(metric_name == "Rate") %>%
      group_by(State = state) %>%
      summarise(`Average Mortality Rate (per 100,000)` = mean(val, na.rm = TRUE) * 100000,
                .groups = "drop") %>%
      arrange(desc(`Average Mortality Rate (per 100,000)`))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, digits = 2)
}

shinyApp(ui = ui, server = server)