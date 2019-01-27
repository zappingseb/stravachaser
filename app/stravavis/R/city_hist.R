

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

#' @import ggplot
city_hist <- function(input, output, session, which_id = 1, city_data=NULL) {
  output$segment_hist <-renderPlot(
    {
      city_data_sets <- city_data()
      data <- city_data_sets %>% dplyr::mutate(distance=as.numeric(distance)/1000) %>% dplyr::filter(
        city == as.character(which_id)
      )

      ggplot(data=data, aes(data$distance)) + theme_classic() +
      geom_histogram(bindwidth=2,color="white", fill="#fc4c02") + 
        xlab("Length of Segment (km)") + ylab("# of segments") + 
        ylim(c(0, dim(city_data_sets)[1]/5))

    }

  )
}
