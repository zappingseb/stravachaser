#' Function to select a city + Range
#' 
#' @author Sebastian Wolf
#' @import shiny 
filteringUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  div(class="stats filters",
      
      tagList(
        # Exchange by https://www.jqueryscript.net/form/jQuery-Plugin-For-Tri-state-Toggle-Switch-Candlestick.html
        selectInput(ns("gender"), "Gender", c(
          "Male" = "m",
          "Female" = "f",
          "both" = "a"
        ),selected = "both"),
        sliderInput(ns("distance"),"Segment length (km)",
                    min=0.05,
                    max=30,
                    value = c(0.5,3)
        ),
        sliderInput(ns("elevation"),"Elevation gain factor",
                    min=0,
                    max=5,
                    value=0
        ),
        sliderInput(ns("chaser"),"# of Athletes on segment",
                    min=1,
                    max=100,
                    value=c(10,100)
        ),
        selectInput(
          ns("score"),
          "Evaluation function",
          choices = c("Average"="avg","Median"="med"),
          selected = "med"
          
        )
      )#taglist
  )#div
  
}

filtering<- function(input, output, session) {
  
  filters <- eventReactive({
    input$gender
    input$distance
    input$elevation
    input$chaser
    input$score
  },{
    
    list(
      gender = input$gender,
      distance = input$distance,
      elevation = input$elevation,
      chaser = input$chaser,
      score = input$score
    )
    
  })
  return(filters)
}