# Shiny Module called filtering - pure input module

#' Function to select a the filters of the statistics
#' 
#' This function creates an output that contains a
#' \itemize{
#' \item{gender}{ male, femal, both}
#' \item{distance}{ length of the segment}
#' \item{elevation}{ 0-5 = Elevation score}
#' \item{chaser}{ # of participants in Top30}
#' \item{score}{ Whether the statistics are calculated by means or medians}
#' }
#' 
#' @param id \code{character} ID of the object inside shiny app
#' 
#' @author Sebastian Wolf
#' @import shinycandlestick
#' @import shiny 
filteringUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  div(class="stats filters",
      
      fluidRow(
        column(4,
               tags$label("Gender"),
               shinycandlestick::CandleStick(ns("gender"),
                                             left = c('f'='f182'),
                                             right = c('m'='f183'),
                                             default = c('a'='f22d'))
        ),
        column(4,
               selectInput(
                 ns("score"),
                 "Evaluation function",
                 choices = c("Average"="avg","Median"="med"),
                 selected = "med"
                 
               )
        ),
        column(4,
               sliderInput(ns("distance"),"Segment length (km)",
                           min=0.05,
                           max=30,
                           value = c(0.5,3)
               )
        )),
      fluidRow(
        column(4,
               sliderInput(ns("elevation"),"Elevation gain factor",
                           min=0,
                           max=5,
                           value=0
               )
        ),
        column(4,
               sliderInput(ns("chaser"),"Minimum # of Athletes on segment",
                           min=1,
                           max=60,
                           value=10
               )
        )
        
      )#fluidRow
      
      
      
  )#div
  
}

#' Compile a list of filters from such inputs - shiny module
#' 
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' 
#' @return filters \code{reactive} list of filters
#' 
#' @author Sebastian Wolf
#' @import shiny
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