library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(readr)  # For reading the coordinates file

# Load dataset
data <- read.csv("worldometer_coronavirus_daily_data.csv")

# Load coordinates file
coordinates <- read_csv("country_coordinates.csv") %>%
  rename(country = Country)

# Preprocess data
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# Summarize data for total cumulative cases globally
cumulativeCases <- data %>%
  group_by(date) %>%
  summarize(
    total_cumulative_cases = sum(cumulative_total_cases, na.rm = TRUE),
    .groups = "drop"
  )
cumulativeDeaths <- data %>%
  group_by(date) %>%
  summarize(
    total_cumulative_deaths = sum(cumulative_total_deaths, na.rm = TRUE),
    .groups = "drop"
  )
# Summarize data for total cases by country
totalCasesByCountry <- data %>%
  group_by(country) %>%
  summarize(total_cases = max(cumulative_total_cases, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_cases)) %>%
  mutate(
    truncated_country = ifelse(
      nchar(country) > 18,
      paste0(substr(country, 1, 17), "..."),
      country
    )
  )


# Get the latest data for mapping
latestData <- data %>%
  group_by(country) %>%
  filter(date == max(date)) %>%
  summarize(
    cumulative_cases = max(cumulative_total_cases, na.rm = TRUE),
    daily_cases = max(daily_new_cases, na.rm = TRUE),
    .groups = "drop"
  )

# Merge latest data with coordinates
latestData <- latestData %>%
  left_join(coordinates, by = "country") %>%
  filter(!is.na(Latitude) & !is.na(Longitude))  # Remove rows with missing coordinates

# UI
ui <- fluidPage(
  titlePanel("COVID-19 Interactive Map and Visualizations"),
  fluidRow(
    column(
      4,
      plotOutput("histogramPlot", height = "300px"),  # Cumulative cases bar plot
      plotOutput("deathsPlot", height = "300px")      # Cumulative deaths bar plot
    ),
    column(
      4,
      leafletOutput("worldMap", height = "600px")   # World map
    ),
    column(
      4,
      div(
        style = "height: 600px; overflow-y: scroll; border: 1px solid #ccc; padding-right: 20px;",
        plotOutput("barPlot", height = "10000px", width = "800px")  # Adjust plot  
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive value for selected country
  selectedCountry <- reactiveVal(NULL)
  
  # Left-side bar plot for cumulative cases
  output$histogramPlot <- renderPlot({
    if (is.null(selectedCountry())) {
      ggplot(cumulativeCases, aes(x = date, y = total_cumulative_cases)) +
        geom_bar(stat = "identity", fill = "#00B7F2", alpha = 0.7) +
        labs(
          title = "Cumulative Cases Globally",
          x = NULL, y = NULL
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"))
    } else {
      countryData <- data %>% filter(country == selectedCountry())
      
      ggplot(countryData, aes(x = date, y = cumulative_total_cases)) +
        geom_bar(stat = "identity", fill = "#00B7F2", alpha = 0.7) +
        labs(
          title = paste("Cumulative Cases in", selectedCountry()),
          x = NULL, y = NULL
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"))
    }
  })
  
  # Second plot for cumulative deaths
  output$deathsPlot <- renderPlot({
    if (is.null(selectedCountry())) {
      ggplot(cumulativeDeaths, aes(x = date, y = total_cumulative_deaths)) +
        geom_bar(stat = "identity", fill = "#00B7F2", alpha = 0.7) +
        labs(
          title = "Cumulative Deaths Globally",
          x = NULL, y = NULL
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"))
    } else {
      countryData <- data %>% filter(country == selectedCountry())
      
      ggplot(countryData, aes(x = date, y = cumulative_total_deaths)) +
        geom_bar(stat = "identity", fill = "#00B7F2", alpha = 0.7) +
        labs(
          title = paste("Cumulative Deaths in", selectedCountry()),
          x = NULL, y = NULL
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"))
    }
  })
  
  # World map
  output$worldMap <- renderLeaflet({
    leaflet(latestData) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~sqrt(cumulative_cases) / 500,
        color = "#00B7F2",
        fillOpacity = 0.6,
        label = ~country,
        layerId = ~country
      ) %>%
      fitBounds(
        lng1 = min(latestData$Longitude, na.rm = TRUE), 
        lat1 = min(latestData$Latitude, na.rm = TRUE),
        lng2 = max(latestData$Longitude, na.rm = TRUE), 
        lat2 = max(latestData$Latitude, na.rm = TRUE)
      )
  })
  
  # Bar plot for total cases by country
  output$barPlot <- renderPlot({
    ggplot(totalCasesByCountry, aes(x = total_cases, y = reorder(truncated_country, total_cases))) +
      geom_bar(stat = "identity", colour = "grey", fill = "#00B7F2", alpha = 0.7, width = 0.6) +
      geom_text(
        aes(label = scales::comma(total_cases)),
        hjust = -0.2,
        size = 3
      ) +
      labs(x = NULL, y = NULL) +
      theme_void() +
      theme(
        axis.text.y = element_text(size = 8, face = "bold", hjust = 1),  
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(5, 420, 5, 5)
      ) +
      coord_cartesian(clip = "off")
  })
  
  # Update selectedCountry when clicking on a bubble
  observeEvent(input$worldMap_marker_click, {
    selectedCountry(input$worldMap_marker_click$id)
  })
  
  # Reset selectedCountry when clicking on the map background
  observeEvent(input$worldMap_click, {
    if (is.null(input$worldMap_marker_click)) {
      selectedCountry(NULL)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
