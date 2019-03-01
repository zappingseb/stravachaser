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
        tags$h1("Additional options to challange the cities.")
        
      ),
      fluidRow(
        column(4,
               tags$label("Gender of cyclists"),
               div(shinycandlestick::CandleStick(ns("gender"),
                                             left = c('f'='f182'),
                                             right = c('m'='f183'),
                                             default = c('a'='f22d'))),
               HTML("<p>
                         Selecting a certain gender will only load scoring results
from STRAVA segments of the specific gender leaderboard. You
can read on this feature in the 
<a href='https://developers.strava.com/docs/reference/#api-Segments-getLeaderboardBySegmentId'>STRAVA API</a>.</p>")
        ),
        div(style="display:none",
            textInput(ns("name_of_gender"),label="hidden",value=ns("gender"))
        ),
        column(4,
               selectInput(
                 ns("score"),
                 "Evaluation function",
                 choices = c("Average"="avg","Median"="med"),
                 selected = "med"
                 
               ),
               
               HTML("<p>
                         The STRAVA leaderboards are evaluated for this app. Each leaderboard contains 30 - 60 cyclists. Here
                         you can select if those shall be evaluated by median or average. All segments medians or averages
                         willl be summarized by the same function again for the whole city.</p>"
               )
        ),
        column(4,
               sliderInput(ns("distance"),"Segment length (km)",
                           min=0.05,
                           max=30,
                           value = c(0.5,3)
               ),
               HTML("<p>
                         STRAVA segments are little race tracks inside the cities. They can have different lenght. People
                         do chase such segments. This means they speed up for a short period of time to just win the segment.
                         For long segments this is basically not possible, really. So this is why this feature is important.</p>"
               )
        )),
      fluidRow(
        column(4,
               sliderInput(ns("elevation"),"Elevation gain factor",
                           min=0,
                           max=5,
                           value=0
               ),
               HTML("<p>
                         As you might know, some cities are quite flat, others have really steep climbs. This is why this
                         app contains a correction factor for hills. The speed gets corrected by this factor for each
                         segment as 'speed = elevation_gain_factor * 0.1 * climb (in meters of the segment)'.</p>"
               )
        ),
        column(4,
               sliderInput(ns("chaser"),"Minimum # of Athletes on segment",
                           min=1,
                           max=60,
                           value=10
               ),
               HTML("<p>
                         Some STRAVA segments are not really frequently used. To exclude those you can use this slider
                         to remove segments with less than a certain number of cyclists on the leaderboard.</p>"
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
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query[['chaser']])) {
      updateSliderInput(session, "chaser", value = query[['chaser']])
    }
    
    if (!is.null(query[['elevation']])) {
      updateSliderInput(session, "elevation", value = query[['elevation']])
    }
    if (!is.null(query[['score']])) {
      updateSliderInput(session, "score", value = query[['score']])
    }
    if (!is.null(query[['distance']])) {
      updateSliderInput(session, "distance", value = 
                          as.numeric(stringr::str_extract(
                            strsplit(query[['distance']],",")[[1]],
                            "\\d{1,5}\\.{0,5}\\d{0,5}"))
      )
    }
    if (!is.null(query[['gender']])) {
      if(query[['gender']]=="f"){
        
        shinycandlestick::updateCandleStick(inputId = input$name_of_gender, value = "off")
      }
      if(query[['gender']]=="m"){
        
        shinycandlestick::updateCandleStick(input$name_of_gender, "on")
      }
      if(query[['gender']]=="a"){
        shinycandlestick::updateCandleStick(input$name_of_gender, "default")
      }
    }
    
    
  })
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