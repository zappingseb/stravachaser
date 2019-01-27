
#' Function to select a city + Range
#' 
#' @author Sebastian Wolf
#' @import shiny 
scoreTextUI <- function(id, label = "CSV file",selected = "London") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tags$h1(
    textOutput(ns("score_text_out"))
  )
  
}


#' Module server function
score_text <- function(input, output, session, scores = NULL,city_data=NULL) {
  
  output$score_text_out <- renderText({
    city_names <- unique(city_data()$city_name)
    score_table <- scores()
    
    paste0(
    city_names[which(score_table$score_new==max(score_table$score_new,na.rm=T))],
    " is the Winner!")
  })
  
}

