#' UI to create a share link from filters
#' 
#' @export
#' 
#' @param id character value of ID of this UI
#' 
shareUI <- function(id){
  
  ns <- NS(id)
  
  div(class="share-ui",
      actionButton(ns("share"),HTML("create / update shareable link <i class='fas fa-sync-alt'></i>")),
      conditionalPanel(
        condition = paste0("input['",ns("share"), "']"),
        textAreaInput(ns("shareLink"), label = "Share Link (already copied to clipboard)", value = "")
      )
  )
  
  
}
#' UI to create a share link from filters
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session 
#' @export
#' 
#' @param filters reactive giving a list of filters used to modify the data
#' 
#' @param id character value of ID of this UI
#' 
share <- function(input, output, session, filters){
  
  
  # In case the Share Link button is clicked:
  observeEvent(input$share,{
    
    url_to_send <- paste0(
      
      session$clientData$url_hostname
    )
    
    if(session$clientData$url_port!=""){
      url_to_send <- paste0(url_to_send,":",session$clientData$url_port)
    }
    url_to_send <- paste0(url_to_send,session$clientData$url_pathname)
    
    all_filters <- filters()
    
    # Paste URL parameters from filters
    url_to_send <- paste0("https://",url_to_send,"?",paste(names(all_filters),all_filters,sep="=",collapse="&"))
    
    updateTextAreaInput(session, "shareLink", value = url_to_send)
    
    # Select and Copy the URL inside a textarea
    shinyjs::runjs(
      "setTimeout(function () {$('textarea').select();document.execCommand('copy');},500)"
    )  
    
  }
  )
  
}