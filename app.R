library(shiny)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(dplyr)
library(readr)  # For reading the coordinates file
library(plotly)

# Load dataset
data <- read.csv("worldometer_coronavirus_daily_data.csv")



# Load coordinates file
coordinates <- read_csv("country_coordinates.csv") %>%
  rename(country = Country)

# Preprocess data
data$date <- as.Date(data$date, format = "%Y-%m-%d")

data <- data %>%
  mutate(across(everything(), ~ ifelse(is.na(.) | . < 0, 0, .)))

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

# Summarize data for total Deaths by country
totalDeathsByCountry <- data %>%
  group_by(country) %>%
  summarize(total_deaths = max(cumulative_total_deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_deaths)) %>%
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
    cumulative_deaths = max(cumulative_total_deaths, na.rm = TRUE),
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
  titlePanel("Global COVID-19 Tracker"),
  fluidRow(
    column(
      4,
      plotOutput("histogramPlot", height = "300px"),  # Cumulative cases bar plot
      plotOutput("deathsPlot", height = "300px")      # Cumulative deaths bar plot
    ),
    column(
      4,
      # Dropdowns above the map
      div(
        fluidRow(
          column(
            6,  # First dropdown: "New" vs "Cumulative"
            selectInput(
              inputId = "dataType",
              label = "Cumulative or New",
              choices = c("Cumulative" = "cumulative", "New" = "new"),
              selected = "cumulative"
            )
          ),
          column(
            6,  # Second dropdown: "Positive Cases" vs "Deaths"
            selectInput(
              inputId = "metric",
              label = "Positive cases or deaths",
              choices = c("Positive Cases" = "cases", "Deaths" = "deaths"),
              selected = "cases"
            )
          )
        ),
        style = "margin-bottom: 10px;"  # Space between dropdowns and map
      ),
      # Map container
      div(
        style = "height: 500px;",
        leafletOutput("worldMap", height = "500px")
      )
    ),
    column(
      4,
      div(
        style = "height: 600px; overflow-y: scroll; border: 1px solid #ccc; padding-right: 20px;",
        plotlyOutput("interactiveBarPlot", height = "10000px", width = "400px")  # Interactive bar plot  
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive value for selected country
  selectedCountry <- reactiveVal(NULL)
  # Reactive value for selected metric used in left plots
  selectedType <- reactive({
    if (input$dataType == "cumulative") {
      list(
        casesData = cumulativeCases,
        deathsData = cumulativeDeaths,
        total_data = "total_cases",  # Use the column name as a string
        title_text = "Cumulative Positive Cases \nSelect a country to see details"
      )
    } else {
      list(
        casesData = newCases,
        deathsData = newDeaths,
        total_data = "total_deaths",  # Use the column name as a string
        title_text = "Cumulative Deaths \nSelect a country to see details"
      )
    }
  })
  # Left-side bar plot for cumulative cases
  output$histogramPlot <- renderPlot({
    if (is.null(selectedCountry())) {
      ggplot(casesData, aes(x = date, y = total_cumulative_cases)) +
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
        scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"))
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
        scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"))
    }
  })
  
  # World map
  output$worldMap <- renderLeaflet({
    leaflet(latestData, options = leafletOptions(
      dragging = FALSE,            # Disable dragging the map
      zoomControl = FALSE,         # Disable zoom control buttons
      scrollWheelZoom = FALSE,     # Disable zooming with mouse wheel
      doubleClickZoom = FALSE,     # Disable zooming with double click
      boxZoom = FALSE              # Disable box zooming
      )) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        (if(input$metric == "cases"){
          radius = ~sqrt(cumulative_cases)/500
        } else{
          radius = ~sqrt(cumulative_deaths)/150
        }), 
        color = "#00B7F2",
        fillOpacity = 0.6,
        label = ~country,
        layerId = ~country
      ) %>%
      setView(
        lng = 0,            # Longitude for the center of the map
        lat = 0,           # Latitude for the center of the map
        zoom = 1.1            # Default zoom level
      )
  })

  # Reactive value for selected metric used in right bar plot
  selectedMetric <- reactive({
    if (input$metric == "cases") {
      list(
        totalDataByCountry = totalCasesByCountry,
        total_data = "total_cases",  # Use the column name as a string
        title_text = "Cumulative Positive Cases \nSelect a country to see details"
      )
    } else {
      list(
        totalDataByCountry = totalDeathsByCountry,
        total_data = "total_deaths",  # Use the column name as a string
        title_text = "Cumulative Deaths \nSelect a country to see details"
      )
    }
  })
  
  # Bar plot for total cases by country (interactive using plotly)
  output$interactiveBarPlot <- renderPlotly({
    metricData <- selectedMetric()
    plot_ly(
      data = metricData$totalDataByCountry,
      x = ~get(metricData$total_data),
      y = ~reorder(truncated_country, get(metricData$total_data)),
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#00B7F2'),
      hoverinfo = ~paste('Country:', truncated_country, '<br>Total Cases:', get(metricData$total_data)),
      source = 'barplot',
      hoverlabel = list(
        bgcolor = 'white',          # Background color of the label
        font = list(
          color = 'black',           # Text color inside the label
          size = 14,                # Font size
          family = 'Arial'          # Font family
        )
      )
    ) %>%
      layout(
        title = metricData$title_text,
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        margin = list(r = 5, t = 70, l = 120)  # Adjust margin for long country names
      )%>%
      config(displayModeBar = FALSE)  # Disable the toolbar
  })
  
  # Observe clicks on the bar plot
  observe({
    barClick <- event_data("plotly_click", source = "barplot")
    if (!is.null(barClick)) {
      clickedCountry <- totalCasesByCountry$country[barClick$pointNumber + 1]  # Adjust for indexing
      selectedCountry(clickedCountry)
    }
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
