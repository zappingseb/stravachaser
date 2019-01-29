#' launches the stravachaser app
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' @import shiny
#'


# wrapper for shiny::shinyApp()
launchApp <- function() {
  shinyApp(ui = ui, server = server)
}

#' UI function of the stravachaser app
#' 
#' Function visualizing a shiny app for strava city comparison
#' 
#' @import shiny
#' @importFrom shinyjs toggle useShinyjs
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
ui <- function(){
  
  shiny::navbarPage(
    useShinyjs(), 
    tabPanel("City vs City",
             progressContainerUI("stravachaser-progress"),
             attachstravavisDep(scoreTextUI("scoretext"),
                                dependency = c("shiny_style.css","style.css","simple-skillbar.css"),
                                dependency_script = c("simple-skillbar.js","barchart-binding.js","scoremessage.js")),
             fluidRow(
               fluidRow(
                 column(6,citySelect("city1")),
                 column(6,citySelect("city2", selected="Paris"))
               ),
               fluidRow(
                 column(6,cityMap("city_map1")),
                 column(6,cityMap("city_map2"))
               ),
               fluidRow(
                 column(12,cityScoreUI("city_score"))
               ),
               fluidRow(
                 column(12,cityCountUI("city_count"))
               )
             )
    ),#city vs city
    tabPanel("Statistics",
             
             fluidRow(
               column(12,tags$h1("Change statistics")),
               column(12,filteringUI("statsfilter"),style="text-align:center")
             )
    )
  )
}


#' Shiny server function of stravavis
#' 
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session 
#' 
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
server <- function(input, output, session) {
  
  shiny::addResourcePath("images", system.file("www/images", package="stravachaser"))
  addResourcePath('datasets', system.file('data', package='datasets'))
  
  #withProgress(message = 'Getting strava data ',value=0.5,
  #  data("all_data_table_strava")
  # )
  
  # Call twice the city module = MAP + Radius + Choice
  city_map1  <- callModule(city, "city1")
  city_map2  <- callModule(city, "city2")
  
  # Render the statistic filtering module
  stats_filters <- callModule(filtering,"statsfilter")
  
  # Create a reactive from the two city data modules
  city_data <-  reactive({
      showProgress("stravachaser-progress","Collecting data",10)
      data <- dplyr::bind_rows(
        city_statistic(filters=stats_filters(), city_data = city_map1()),
        city_statistic(filters=stats_filters(), city_data = city_map2()),
        .id = "city"
      )
      hideProgress("stravachaser-progress")
      return(data)
  })
  
  
  # Visualize the segments per city at a map
  callModule(module = city_map, id = "city_map1", city_data = city_data, city_id = 1)
  callModule(module = city_map, id = "city_map2", city_data = city_data, city_id = 2)
  
  # Visualize the segments per city
  callModule(city_count,"city_count",city_data=city_data)
  
  # Visualize the Scores for each city
  score_table <- callModule(city_score,"city_score",city_data=city_data, stats = stats_filters)
  
  # Visualize which city won
  callModule(score_text,"scoretext",scores = score_table, city_data = city_data)
}