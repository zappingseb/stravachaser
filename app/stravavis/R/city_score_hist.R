

#' @importFrom htmltools tagList tags singleton
cityhistScoreUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

    div(class="city hist",
      plotOutput(
        ns("score_hist")
      )# plotOutput
  )#div
  

}

#' @import ggplot
city_score_hist <- function(input, output, session, city_data=NULL, stats=NULL) {
  output$score_hist <-renderPlot(
    {
      city_data_sets <- city_data()
      summary_tab <- city_data_sets %>%
        dplyr::mutate( ints = cut(as.numeric(distance)/1000 ,breaks = c(0,0.2,0.5,1,2.5,5,10,Inf))) %>% 
        dplyr::group_by(ints,city)
      summary_tab <- switch(stats()$score,
                            "med" = summary_tab %>% dplyr::summarise( median_score = median(score,na.rm=T)),
                            "avg" = summary_tab %>% dplyr::summarise( median_score = mean(score,na.rm=T))
                            ) 
    
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
      left_right_plot(label=labels,left=left,right=right)

    }

  )
}
