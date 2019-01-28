# City Histogram Module - shiny output module


#' UI for a Histogram for segment length
#' 
#' @param id ID
#' @importFrom htmltools tagList tags singleton
cityhistUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

    div(class="city hist",
      plotOutput(
        ns("segment_hist")
      )# plotOutput
  )#div
  

}

#' City Histogram length of segments module
#' 
#' @param which_id \code{numeric} 1 or 2
#' @param city_data \code{reactive - data.frame} reactive giving a data frame
#'    with a distance column and a city column
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session 
#'   
#' @return Set the \code{output$segment_hist} part of the module to
#'    contain a ggplot histogram of the segments binned by length
#' 
#' @import ggplot2
city_hist <- function(input, output, session, which_id = 1, city_data=NULL) {
  output$segment_hist <-renderPlot(
    {
      city_data_sets <- city_data()
      data <- city_data_sets %>% dplyr::mutate(distance=as.numeric(.data$distance)/1000) %>% dplyr::filter(
        city == as.character(which_id)
      )

      ggplot(data=data, aes(data$distance)) + theme_classic() +
      geom_histogram(bindwidth=2,color="white", fill="#fc4c02") + 
        xlab("Length of Segment (km)") + ylab("# of segments") + 
        ylim(c(0, dim(city_data_sets)[1]/5))

    }

  )
}
