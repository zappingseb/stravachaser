library(shiny)
library(leaflet)
library(ggmap)
library(prettymapr)
library(sp)
library(rgdal)
library(RANN)
library(tibble)
library(shinyjs)

source("city.R")
source("filtering.R")
source("city_statistic.R")
source("score_text.R")

devtools::load_all("stravavis")

ui <- fluidPage(theme="style.css",
  useShinyjs(), 
  fluidRow(
    
    column(12,scoreTextUI("scoretext"))
    
  ),
  fluidRow(
    column(6,citySelect("city1", "Select your City")),
    column(6,citySelect("city2", "Select your City",selected="Paris"))
  ),
  fluidRow(
    column(12,cityScoreUI("city_score"))
  ),
  fluidRow(
    column(12,cityCountUI("city_count"))
  ),
  fluidRow(
    column(12,tags$h1("Change statistics")),
    column(12,filteringUI("statsfilter"),style="text-align:center")
  )
  
)

server <- function(input, output, session) {
  city_map  <- callModule(city, "city1")
  city_map2 <- callModule(city, "city2")
  
  stats_filters <- callModule(filtering,"statsfilter")
  
  city_data <- reactive({
    dplyr::bind_rows(
      city_statistic(filters=stats_filters(), city_data = city_map()),
      city_statistic(filters=stats_filters(), city_data = city_map2()),
      .id = "city"
    )
  })
  
  callModule(city_count,"city_count",city_data=city_data)
  
  score_table <- callModule(city_score,"city_score",city_data=city_data, stats = stats_filters)
  
  callModule(score_text,"scoretext",scores = score_table, city_data = city_data)
  
}

shinyApp(ui, server)