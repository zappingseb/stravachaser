# city_score_histogram module - pure output module


#' City Score Histogram UI Module
#' 
#' @param id \code{character} ID of this module
#' @export
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
cityhistScoreUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

    div(class="city hist",
      barChartOutput(
        ns("score_hist")
      )# plotOutput
  )#div
  

}
#' City Score Histogram Module
#' 
#' Create a left right plot for the scores of segments binned by the
#' length of the segments
#' 
#' @export
#' 
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session 
#' @param city_data \code{reactive} representing strava crawled data from at
#'  least two different cities
#' @param stats \code{reactive} A reactive element representing the outcome
#' of the \link{filtering} module that contains the filters to be used for
#' statistics. 
#' 
#' @importFrom dplyr summarise group_by mutate summarise
#' @import ggplot2
#' @importFrom rlang .data
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
city_score_hist <- function(input, output, session, city_data=NULL, stats=NULL) {
  output$score_hist <- renderBarChart(
    {
      city_data_sets <- city_data()
      
      # bin the data by the length of segments
      summary_tab <- city_data_sets %>%
        dplyr::mutate( ints = cut(as.numeric(.data$distance)/1000 ,breaks = c(0,0.2,0.5,1,2.5,5,10,Inf))) %>% 
        dplyr::group_by(.data$ints,.data$city)
      
      # summarize the scores for bins by the statistic wanted
      summary_tab <- switch(stats()$score,
                            "med" = summary_tab %>% dplyr::summarise( median_score = median(.data$score,na.rm=T)),
                            "avg" = summary_tab %>% dplyr::summarise( median_score = mean(.data$score,na.rm=T))
                            ) 
    
      # Create a left right plot from the summary
      labels <- rev(as.character(unique(summary_tab$ints)))
      left <- c()
      right <- c()
      for (label in labels) {
        
        line_nr <- which(summary_tab$ints==label & summary_tab$city=="1")
        
        if(length(line_nr)>0){
          left<-c(left,round(summary_tab$median_score[line_nr],3))
        }else{
          left<-c(left,0)
        }
        
        line_nr <- which(summary_tab$ints==label & summary_tab$city=="2")
        if(length(line_nr)>0){
          right<-c(right,round(summary_tab$median_score[line_nr],3))
        }else{
          right<-c(right,0)
        }
      }
      left[which(is.na(left))] <- 0
      right[which(is.na(right))] <- 0
      
      clean_labels <- function(x){
        strsplit(x,",")[[1]] %>% paste(
          "km"
          
        ) %>%
          paste0(collapse = " - ") %>%
          
          stringr::str_replace_all(pattern="\\]|\\(",replacement = "")
      }
      
      #' Plot left/right statistics
      jsonlite::toJSON(
        list(
          left = left,
          right = right,
          label = vapply(labels, FUN = clean_labels,FUN.VALUE = character(1)),
          unit = "(~km/h)"
        )
        
      )

    }

  )
}
