#' City Selection Module

#' UI Function to select a city + Range
#' 
#' @param id \code{character} ID of the element
#' @param selected \code{character} Name of the city that shall be selected
#' @param selections \code{character}  named vector to allow cities to go into
#'    the select input
#' 
#' @author Sebastian Wolf
#' @import shiny 
citySelect <- function(id, selected = "London", selections = c(
  "London" = "London",
  "Paris" = "Paris",
  "Berlin" = "Berlin"
)) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    selectInput(ns("name"), "City Name", choices = selections ,selected = selected),
    sliderInput(ns("radius"), label = "Radius (km)",min = 1,max=30,value=20)
   # textOutput(ns("mytext"))
  )
  
}


#' city Module server function
#' 
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session 
#' 
#' @return The data for all segments of the city
#' 
#' @importFrom prettymapr geocode
#' @importFrom glue glue
#' @import leaflet
#' @importFrom rlang .data
city <- function(input, output, session) {
  
  # Get the selected cities location
  points <- eventReactive(input$name, {
    if(is.null(input$name)){name <- "London"}else{ name <- input$name}
    prettymapr::geocode(name)[1,c("lon","lat")]
    
  }, ignoreNULL = FALSE)
  
  # Get the radius in km
  radius <- eventReactive(input$radius, {
    input$radius*1000
  })
  
  # Derive the Strava segments indeces within the radius
  segment_indeces <- eventReactive({input$name
    input$radius},{
    
    segment_indeces <- calc_distance_city_stravasegments(input$name, radius())
    
    segment_indeces <- intersect(
      segment_indeces,
      which(as.numeric(stravachaser::all_data_table_strava$average)>0)
    )
    return(segment_indeces)
  })
  
  # Derive the data of the strava segments within the radius
  segment_loc <- reactive(
    return(stravachaser::all_data_table_strava[segment_indeces(),c("lng","lat","id","name")])
  )
  
  segment_data <- reactive({
    output_table <- stravachaser::all_data_table_strava[segment_indeces(),]
    output_table$city_name <- input$name
    output_table$radius <- radius()
    
    return(output_table)
  })
  
  # Derive the # of segments within the radius
  segment_nr <- reactive(
    length(segment_indeces())
  )
  
 # output$mytext <- renderText(paste0("You see ",segment_nr()," segments."))
  
  return(
    segment_data
  )
}

