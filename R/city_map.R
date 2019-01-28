# city_map Module


#' UI to contain a leaflet map
#'
#' @export
#'  
#' @param id ID of the map
cityMap <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    leaflet::leafletOutput(ns("mymap"))
  )
  
}

#' Module Server to provide a city map
#' 
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session 
#' 
#' @export
#' 
#' @param city_data \code{data.frame} Data frame with segment data from two cities
#' @param city_id \code{numeric} City to take from segment_data (1 | 2)
#' 
city_map <- function(input, output, session, city_data = NULL, city_id=NULL) {
  # Create a map with a radial layer
  map <- reactive({
    
    segment_data_rendered <- city_data()
    
    city_name <- unique(segment_data_rendered$city_name)[city_id]
    
    radius <- unique(segment_data_rendered$radius)[city_id]
    
    if(is.na(radius)){
      radius <- unique(segment_data_rendered$radius)[1]
    }
    
    marker_list <- prettymapr::geocode(city_name)[1,c("lon","lat")]
    new_zoom <- 9
    
    if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
    
    segment_map(segment_data = segment_data_rendered, radius = radius, zoom=new_zoom, marker_list = marker_list)
    
  })
  
  output$mymap <- renderLeaflet(map())
}
