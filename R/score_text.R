
#' Function to select a city + Range
#' 
#' @param id ID
#' 
#' @author Sebastian Wolf
#' @export
scoreTextUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  div(id=ns("winner-wrapper"),class="andthewinneris",
          
    htmlOutput(ns("winnerimage"))
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
  
  observeEvent({
    scores()
    },{
    shinyjs::hide('winner-wrapper')
      Sys.sleep(1)
    shinyjs::show('winner-wrapper',anim = TRUE, animType = "fade", time = 1)
  })
    
  winner <- reactive({
    city_names <- unique(city_data()$city_name)
    score_table <- scores()
    city_names[which(score_table$score_new==max(score_table$score_new,na.rm=T))]
  })
  
  
  output$winnerimage <- renderUI({
    HTML(paste0(
      "<div class='bgimage' style='background-image:url(./images/",tolower(winner()), ".jpg)'></div>",
      "<div class='scoremessage'  >",
      winner()," is the Winner!</div>"
      )
    )
  })
}

