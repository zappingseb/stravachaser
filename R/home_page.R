#' Modal dialog on Front Page
#' 
#' @param id \code{character} Id of the item
#' 
#' @author Sebastian Wolf
#' @import shiny 
homePageUI <- function(id) {
  
  
  
}

homepage <- function(input, output, session){
  
  ns <- NS("startmodal")
  
  in_dialog <- modalDialog(
    title="",
    div(class="home-page",
        tags$h1("City Cycle Race"),
        tags$h2("Use big data to find which city has the fastest cyclists?"),
        actionButton(ns('race'),'Let London race against Paris.'),
        tags$br(),tags$br(),
        actionButton(ns("config"), 'Configure an own race'),
        actionButton(ns("about"), 'About this project')
    ),
    easeClose = F
  )
  showModal(in_dialog)
  
  observeEvent(input$race,
               {
                 removeModal()
               })
  observeEvent(input$config,
               {
                 shinyjs::runjs('$( ".stravachaserdropdown .btn-info" ).trigger( "click" )');
                 removeModal()
               })
  observeEvent(input$about,
               {
                shinyjs::runjs("activate_panel('about');")
                 removeModal()
               })
}
