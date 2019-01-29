progressContainerUI <- function(id,width=10,message="Waiting...") {
  # Create a namespace function using the provided id
  
  HTML(
    glue::glue(
      '<div id="{id}" style="display:block" class="stravachaser progress">
        <div class="message">{message}</div>
        <div class="bar100"><div class="bar loading" style="width:{width}%"><div class="bg"></div></div></div>
      </div>
    '))
  
}


updateProgress <- function(id, message, percent=10){
  
  shinyjs::runjs(glue::glue("$('#{id}').find('.message').text('{message}');"))
  shinyjs::runjs(glue::glue("$('#{id}').find('.bar').css('width','{percent}%');"))
  
}

hideProgress <- function(id){
  shinyjs::runjs(glue::glue("$('#{id}').find('.bar').css('width','{100}%');"))
  shinyjs::hide(id,anim = TRUE,animType = 'fade')
  
}

showProgress <- function(id, message="Waiting", percent=10){
  
  shinyjs::show(id)
  
  updateProgress(id, message, percent)
  
}