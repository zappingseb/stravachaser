

#' UI function to generate a segment overview per city
#' 
#' @param id ID
#' @export
#' 
cityCountUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  mytag <- 
    div(class="city count",
      plotOutput(
        ns("count_compare")
      )# plotOutput
  )#div
  
  fluidRow(
    tags$h2("Selected Segments"),
    column(12,attachstravavisDep(tag = mytag)),
    column(12,div(style="height:2em")),
    column(12,div(class="city count withhover",
                  
        # Send out a value jsValue to show hide the histograms inside the wrapper          
        onClick=paste0("Shiny.onInputChange('",ns('jsValue'),"',Math.random());"),
        plotOutput(
          ns("count_length")
        )# plotOutput
    )),#div #column
    column(12,HTML("Click the graphic for details")),
    column(12,div(style="height:5em")),
    div(id=ns("city_hist_wrapper"),class="count wrapper",style="display:none",
        column(6,cityhistUI(ns("city_hist1"))),
        column(6,cityhistUI(ns("city_hist2")))
    )#div
  )
}

#' Server function of city_count module
#' 
#' This module shows how many segments can be seen for each city
#' It contains a plot for the average length of a segment.
#' Additionally it contains the \link{city_hist} module twice
#' to show the histogram of the segment length for each city.
#' 
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session 
#' 
#' @param city_data \code{reactive} representing strava crawled data from at
#'  least two different cities
#'  
#' @importFrom dplyr group_by summarise
#' 
#' @importFrom rlang .data
#'  
city_count <- function(input, output, session, city_data=NULL) {
  
  # Left right plot of the # of segments
  output$count_compare <-renderPlot(
    {
      city_data_sets <- city_data()
      data <- city_data_sets %>% dplyr::group_by(.data$city) %>% dplyr::summarise(n = dplyr::n())
      left_right_plot(left=round(data$n[1],3),right=round(data$n[2],3))
      
    }
    
  )
  
  # left right plot of the average length of a segment
  output$count_length <-renderPlot(
    {
      city_data_sets <- city_data()
      data <- city_data_sets %>% dplyr::group_by(.data$city) %>% dplyr::summarise(n = mean(as.numeric(.data$distance)/1000,na.rm=T))
      left_right_plot(label="length of segments (avg)",
                      left=round(data$n[1],3),
                      right=round(data$n[2],3),
                      unit="km")
      
    }
    
  )
  
  # Upon JS Value show or hide the histograms of the segment length
  observeEvent(input$jsValue, {
    
    toggle("city_hist_wrapper")  # toggle is a shinyjs function
    Sys.sleep(0.05)
   
    callModule(city_hist,"city_hist1",which_id = 1, city_data = city_data)
    callModule(city_hist,"city_hist2",which_id = 2, city_data = city_data)
    Sys.sleep(0.1)
    shinyjs::runjs('
                   $(window).scrollTop($(".count.wrapper").offset().top+$(".count.wrapper").children().height()+$(".count.wrapper").height())
                   ')
  })
}


