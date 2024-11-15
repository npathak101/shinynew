# Load necessary libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(dplyr)
library(readxl)
library(reshape2)
library(xgboost)

# Load data
data <- read_excel("/Users/nirvanipathak/Downloads/newdata_cleaned.xlsx")
heatmap_data <- read_excel("/Users/nirvanipathak/Downloads/lotwize@npathak (2)/lotwize_case.xlsx")

# Ensure latitude and longitude are numeric
heatmap_data <- heatmap_data %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Train XGBoost Model (or load a pre-trained model)
set.seed(123)
train_data <- data %>% select(-price)  # Exclude price column for training features
train_labels <- data$price
dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = train_labels)
xgb_model <- xgboost(data = dtrain, max_depth = 6, nrounds = 100, objective = "reg:squarederror")

# Define UI
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .navbar-custom {background-color: #007bff; color: white; font-weight: bold;}
    .navbar-button {width: 100%; padding: 10px; font-size: 16px; color: white; border: none; border-radius: 5px; transition: background-color 0.3s;}
    .navbar-button:hover {background-color: #0056b3;}
    .content-area {padding: 20px; background-color: #f8f9fa; border-radius: 8px; margin-top: 20px;}
    .panel-title {font-weight: 700; color: #343a40;}
    .highlight-text {color: #007bff; font-weight: bold; font-size: 24px;}
    .heatmap-container {padding: 20px; background-color: #ffffff; border: 1px solid #ddd; border-radius: 8px; margin-top: 20px;}
  "))),
  
  h2("Market Overview and Correlation Analysis", style = "text-align: center; color: #343a40; font-weight: bold; margin-top: 20px;"),
  
  fluidRow(
    column(6, actionButton("market_overview", "Market Overview", class = "navbar-button navbar-custom")),
    column(6, actionButton("correlation_analysis", "Correlation Analysis", class = "navbar-button navbar-custom"))
  ),
  
  hr(style = "border-top: 2px solid #ddd; margin-top: 20px;"),
  
  uiOutput("content_area")
)

# Define Server
server <- function(input, output, session) {
  active_tab <- reactiveVal("market_overview")
  
  observeEvent(input$market_overview, { active_tab("market_overview") })
  observeEvent(input$correlation_analysis, { active_tab("correlation_analysis") })
  
  # Reactive prediction for Average Price using XGBoost model
  predicted_price <- reactive({
    req(active_tab() == "market_overview")
    new_data <- data %>% select(-price)  # Create features for prediction
    dnew <- xgb.DMatrix(data = as.matrix(new_data))
    pred <- predict(xgb_model, dnew)
    mean(pred, na.rm = TRUE)  # Get average predicted price
  })
  
  output$content_area <- renderUI({
    if (active_tab() == "market_overview") {
      div(
        fluidRow(
          column(4, wellPanel(
            h4("Predicted Average Price", class = "panel-title"),
            h3(paste0("$", formatC(predicted_price(), format = "f", big.mark = ",", digits = 2)), class = "highlight-text")
          )),
          column(4, wellPanel(
            h4("Most Expensive Neighborhood in CA", class = "panel-title"),
            h3("San Francisco", class = "highlight-text")
          )),
          column(4, wellPanel(
            h4("Area with Highest Percent Change Rate of Price", class = "panel-title"),
            h3("San Diego/Coastal SoCal : +36.26%", class = "highlight-text")
          ))
        ),
        hr(style = "border-top: 1px solid #ddd; margin-top: 20px;"),
        div(
          h4("Price Heat Map", class = "panel-title", style = "text-align: center;"),
          div(leafletOutput("heatmap"), class = "heatmap-container")
        )
      )
    } else if (active_tab() == "correlation_analysis") {
      div(
        h4("Correlation Matrix for Price and Related Variables", class = "panel-title", style = "text-align: center; margin-bottom: 15px;"),
        plotOutput("correlation_matrix"),
        class = "content-area"
      )
    }
  })
  
  output$correlation_matrix <- renderPlot({
    selected_columns <- c('price', 'mortgageZHLRates/thirtyYearFixedBucket/rate', 
                          'priceHistory/0/priceChangeRate', 'lotAreaValue', 'tourViewCount', 
                          'bedrooms', 'zipcode_cluster')
    data_filtered <- data %>% select(all_of(selected_columns))
    corr <- cor(data_filtered, use = "complete.obs")
    ggplot(melt(corr), aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
      labs(fill = "Correlation")
  })
  
  output$heatmap <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng = -122.4194, lat = 37.7749, zoom = 6)
    m <- m %>% addHeatmap(
      lng = heatmap_data$longitude,
      lat = heatmap_data$latitude,
      radius = 10
    )
    m
  })
}

# Run the app
shinyApp(ui, server)
