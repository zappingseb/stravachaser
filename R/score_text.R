
#' Function to select a city + Range
#' 
#' @param id ID
#' 
#' @author Sebastian Wolf
#' @export
scoreTextUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tags$h1(
    textOutput(ns("score_text_out"))
  )
  
}


#' Module server function
#' 
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session 
#' 
#' @param city_data city data to extract the city_names from
#' @param scores scoring table to calculate which city won
#' 
#' @return set the \code{output$score_text_out} of this module
#'  to 'XXX is the winner' while replacing XX with the city
#'  with the higher score in the city_data
#' 
#' @export
score_text <- function(input, output, session, scores = NULL,city_data=NULL) {
  
  output$score_text_out <- renderText({
    city_names <- unique(city_data()$city_name)
    score_table <- scores()
    
    paste0(
    city_names[which(score_table$score_new==max(score_table$score_new,na.rm=T))],
    " is the Winner!")
  })
  
}

