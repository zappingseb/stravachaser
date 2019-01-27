#' Calculate the distance between two places in m
#' 
#' @param points_from \code{data.frame} A data frame with a 'lng' at 'lat' column
#'  containing a list of points from where the distance shall be calculated
#' @param points_to \code{data.frame} A data frame with a 'lng' at 'lat' column
#'  with just a single point that the distance shall be calculated to
#' @import RANN rgdal tibble sp
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de} 
calc_distance <- function(
  points_from = data.frame(lng=c(),lat=c()),
  points_to = data.frame(lng=c(),lat=c())
){
  if(!"lng" %in% names(points_to)){
    stop("longitude needed in 'points_to' as lng")
  }
  if(!"lat" %in% names(points_to)){
    stop("latitude needed in 'points_to' as lat")
  }
  
  ## First define a function from Josh OBrien's answer to convert
  ## a longitude to its UTM zone
  long2UTM <- function(long) {
    (floor((long + 180)/6) %% 60) + 1
  }
  
  ## Assuming that all points are within a zone (within 6 degrees in longitude),
  ## we use the first shop's longitude to get the zone.
  z <- long2UTM(points_from[ceiling(dim(points_from)[1]/2),"lng"])
  
  ## convert the bus lat/long coordinates to UTM for the computed zone
  ## using the other Josh O'Brien linked answer
  bus2 <- points_from[,c("lng","lat")]
  names(bus2) <- c("lng","lat")
  
  coordinates(bus2) <- c("lng", "lat")
  proj4string(bus2) <- CRS("+proj=longlat +datum=WGS84")
  bus.xy <- spTransform(bus2, CRS(paste0("+proj=utm +zone=",z," ellps=WGS84")))
  
  # Create a Berlin Pointsmap
  points_to <- points_to[,c("lng","lat")]
  coordinates(points_to) <- c("lng", "lat")
  proj4string(points_to) <- CRS("+proj=longlat +datum=WGS84")
  berlin.xy <- spTransform(points_to, CRS(paste0("+proj=utm +zone=",z," ellps=WGS84")))
  
  ## Compile the distance between Berlin and all Segments
  res <- nn2(berlin.xy@coords, bus.xy@coords,1)
  return(res)
}

#' Function to select a city + Range
#' 
#' @author Sebastian Wolf
#' @import shiny 
citySelect <- function(id, label = "CSV file",selected = "London") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    selectInput(ns("name"), "City Name", c(
      "London" = "London",
      "Paris" = "Paris",
      "Berlin" = "Berlin"
    ),selected = selected),
    sliderInput(ns("radius"), label = "Radius (km)",min = 1,max=30,value=20),
    leafletOutput(ns("mymap")),
    textOutput(ns("mytext"))
  )
  
}


#' Module server function
#' @importFrom prettymapr geocode
city <- function(input, output, session) {
  
  #' Get the selected cities location
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
    
    points_to <- prettymapr::geocode(input$name)[1,c("lon","lat")]
    points_to$lng <- points_to$lon
    
    distance <- calc_distance(
      points_from = all_data_table_strava,
      points_to = points_to
    )
    segment_indeces <- which(distance$nn.dists <= radius())
    return(segment_indeces)
  })
  
  # Derive the data of the strava segments within the radius
  segment_loc <- reactive(
    return(all_data_table_strava[segment_indeces(),c("lng","lat")])
  )
  
  segment_data <- reactive({
    output_table <- all_data_table_strava[segment_indeces(),]
    output_table$city_name <- input$name
    return(output_table)
  })
  
  # Derive the # of segments within the radius
  segment_nr <- reactive(
    length(segment_indeces())
  )
  
  # Create a map with a radial layer
  map <- reactive({
    
    new_zoom <- 9
    if(!is.null(input$mymap_zoom)) new_zoom <- input$mymap_zoom
    
    icons <- awesomeIcons(
      icon = 'bicycle',
      iconColor = 'black',
      library = 'fa',
      markerColor = "#fc4c02"
    )
    
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                      options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lat =points()$lat,lng=points()$lon, zoom = new_zoom) %>%
      addAwesomeMarkers(data = points(),icon=icons) %>%
      addCircleMarkers(data=segment_loc(),radius = 0.5,color = "#fc4c02") %>%
      addCircles(lat = points()$lat,lng=points()$lon,radius = radius(),color="#fc4c02",fillColor="#e6e6eb")
  })
  
  output$mymap <- renderLeaflet(map())
  output$mytext <- renderText(paste0("You see ",segment_nr()," segments."))
  
  return(
    segment_data
  )
}

