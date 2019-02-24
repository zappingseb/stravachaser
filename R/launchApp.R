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
#' @importFrom shinyWidgets dropdownButton
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
ui <- function(){
  
  shiny::navbarPage(HTML(
    "<div class = 'appname'><i class='fas fa-bicycle'></i> City Race</div>"
  ),
    
                      
    tabPanel("City vs City",
               useShinyjs(), 
             
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Roboto")
             ), 
             div(class="stravachaserdropdown",dropdownButton(
               circle = FALSE,
               tooltipOptions(title = "More statistic options.",placement = "right"),
               icon = icon("gear"), 
               filteringUI("statsfilter"),
               status = "info"
             )),
             progressContainerUI("stravachaser-progress"),
             fluidRow(
               fluidRow(
                 column(12,
                        
                        attachstravavisDep(
                          cityScoreUI("city_score"),
                          dependency = c("shiny_style.css","style.css","simple-skillbar.css","dropdownbutton.css"),
                          dependency_script = c("simple-skillbar.js","barchart-binding.js","scoremessage.js"))
                        
                 )#column
               ),
               fluidRow(
                 column(12,
                        column(6,
                               citySelect("city1"),
                               cityMap("city_map1")
                               
                               ),
                        column(6,citySelect("city2", selected="Paris"),cityMap("city_map2"))
                        
                 )
               ),
               fluidRow(
                 column(12,cityCountUI("city_count"))
               )
             )
    ),
    tabPanel("About",
             about("aboutmod")
             
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
  
  addResourcePath("images", system.file("www/images", package="stravachaser"))
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