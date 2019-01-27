
cityScoreUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  mytag <- 
    div(class="city score count withhover",
        onClick=paste0("Shiny.onInputChange('",ns('jsValue'),"',Math.random());"),
      plotOutput(
        ns("score_compare")
      )# plotOutput
  )#div
  
  fluidRow(
    tags$h2("City Score"),
    column(12,mytag),
    
    div(id=ns("city_hist_wrapper"),class="score wrapper",style="display:none",
        column(12,tags$b("Split by segment length:")),
        column(12,cityhistScoreUI(ns("city_hist_score")))
    )#div
  )
}

city_score <- function(input, output, session, city_data=NULL, stats=NULL) {
  
  score_data <- reactive({
    # CLIMB is calculated as ELEVATION/DISTANCE
    city_data_sets <- city_data() %>% 
      dplyr::mutate(climb = as.numeric(total_elevation_gain)/as.numeric(distance)*100)
    
    stats_filters <- stats()
    
    # SCORE is calculated as MEDIAN + ELEVATION_STATS * 0.5 * CLIMB
    switch(
      stats_filters$score,
      "med" = city_data_sets %>% dplyr::mutate(score = as.numeric(median) + (!!stats_filters$elevation*0.1*climb)),
      "avg" = city_data_sets %>% dplyr::mutate(score = as.numeric(average) + (!!stats_filters$elevation*0.1*climb)),
    )
  })
  
  scores <- reactive({
    
    switch(
      stats()$score,
      "med" = score_data() %>% dplyr::group_by(city) %>% dplyr::summarise(score_new = median(score,na.rm=T)),
      "avg" = score_data() %>% dplyr::group_by(city) %>% dplyr::summarise(score_new = mean(score,na.rm=T)),
    )
  })
  
  output$score_compare <-renderPlot(
    {
      scores <- scores()

      left_right_plot(label="Score (km/h)",left=round(scores$score_new[1],3),right=round(scores$score_new[2],3))
      
    }
    
  )
  
  observeEvent(input$jsValue, {
    
    toggle("city_hist_wrapper")  # toggle is a shinyjs function
    Sys.sleep(0.05)
    
    callModule(city_score_hist,"city_hist_score", city_data = score_data, stats=stats)
    Sys.sleep(0.1)
    shinyjs::runjs('
                   $(window).scrollTop($(".score.wrapper").offset().top+$(".score.wrapper").children().height()+$(".count.wrapper").height())
                   ')
  })
  
  
  return(scores)
}


