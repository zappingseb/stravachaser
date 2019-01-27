
attachstravavisDep <- function(tag) {
  version <- as.character(packageVersion("stravavis")[[1]])
  dep <- htmltools::htmlDependency(
    name = "stravavis", version = version,
    package = "stravavis",
    src = "inst/www", # - Fix upon loading without devtools
    stylesheet = "style.css"
  )
  message("ATTACHED 'style.css' from 'stravavis' package.")
  htmltools::attachDependencies(tag, dep, append = TRUE)
}

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

city_count <- function(input, output, session, city_data=NULL) {
  
  output$count_compare <-renderPlot(
    {
      city_data_sets <- city_data()
      data <- city_data_sets %>% dplyr::group_by(city) %>% dplyr::summarise(n = n())
      left_right_plot(left=round(data$n[1],3),right=round(data$n[2],3))
      
    }
    
  )
  output$count_length <-renderPlot(
    {
      city_data_sets <- city_data()
      data <- city_data_sets %>% dplyr::group_by(city) %>% dplyr::summarise(n = mean(as.numeric(distance)/1000,na.rm=T))
      left_right_plot(label="length of segments (avg)",
                      left=round(data$n[1],3),
                      right=round(data$n[2],3),
                      unit="km")
      
    }
    
  )
  
  observeEvent(city_data,
               {
                 callModule(city_hist,"city_hist1",which_id = 1, city_data = city_data)
                 callModule(city_hist,"city_hist2",which_id = 2, city_data = city_data)
               }
               )
  
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


