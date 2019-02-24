# Shiny Module called 'city_score' - pure output module

#' Score output - Function providing a shiny Output
#' 
#' This function generates 1 plot giving the overall score
#' of two cities. Additionally it can integrate a \link{city_score_hist}
#' 
#' @param id \code{character} ID of the element
#' 
#' @export
#' @author Sebastian Wolf
cityScoreUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  mytag <- 
    div(class="city score count withhover",
        # jsValue used to call the toggle of the detail view
        onClick=paste0("Shiny.onInputChange('",ns('jsValue'),"',Math.random());"),
      barChartOutput(
        id=ns("score_compare"),
        label = ""
      )
  )#div
  
  fluidRow(
    tags$h2("Which city is faster?"),
    div(style="font-size:0.8em",tags$p("You can click the bar chart for details on the race.")),
    column(12,mytag),
    
    div(id=ns("city_hist_wrapper"),class="score wrapper",style="display:none",
        tags$br(),
        column(12,tags$b("Score by segment length:")),
        column(12,cityhistScoreUI(ns("city_hist_score")))
    )#div
  )
}

#' city_score module
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session 
#' 
#' uses \link{calc_score} to calculate the score for each segment
#' 
#' @param city_data \code{reactive} representing strava crawled data from at
#'  least two different cities
#'  
#' @param stats \code{reactive} A reactive element representing the outcome
#' of the \link{filtering} module that contains the filters to be used for
#' statistics. 
#' 
#' @importFrom dplyr mutate filter group_by summarise
#' @importFrom shinyjs toggle
#' @importFrom rlang .data
#' @author Sebastian Wolf
#' @export
city_score <- function(input, output, session, city_data=NULL, stats=NULL) {
  
  # Calculate the scores per segment
  score_data <- reactive({
    # CLIMB is calculated as ELEVATION/DISTANCE
    city_data_sets <- city_data() %>% 
      dplyr::mutate(climb = as.numeric(.data$total_elevation_gain)/as.numeric(.data$distance)*100)
    
    stats_filters <- stats()
    
    calc_score(statistics = stats_filters$score, elevation_factor=stats_filters$elevation, city_data_sets=city_data_sets)
  })
  
  # Calculate the scores per city
  scores <- reactive({
    
    switch(
      stats()$score,
      "med" = score_data() %>% dplyr::group_by(.data$city) %>% dplyr::summarise(score_new = median(.data$score,na.rm=T)),
      "avg" = score_data() %>% dplyr::group_by(.data$city) %>% dplyr::summarise(score_new = mean(.data$score,na.rm=T))
    )
  })
  
  # Output to show left and right the scores
  output$score_compare <- renderBarChart(
    {
      scores <- scores()
      jsonlite::toJSON(
        list(
          left=round(scores$score_new[1],3),
          right=round(scores$score_new[2],3),
          unit = '(~km/h)'
          
        )
      )
      
    }
    
  )
  
  # On jsValue toggle a detail view of the score per segment length
  observeEvent(input$jsValue, {
    
    shinyjs::toggle("city_hist_wrapper")  # toggle is a shinyjs function
    Sys.sleep(0.05)
    
    callModule(city_score_hist,"city_hist_score", city_data = score_data, stats=stats)
    Sys.sleep(0.1)
    #shinyjs::runjs('
    #$(window).scrollTop($(".score.wrapper").offset().top+$(".score.wrapper").children().height()+$(".count.wrapper").height())')
  })
  
  
  return(scores)
}


