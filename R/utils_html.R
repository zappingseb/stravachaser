# Utils for shiny
#
#
#

#' Attach Stylesheet dependencies and js dependencies
#' 
#' @param tag a shiny element that the dependencies shall be added to
#' @param dependency \code{character} A list of css files located in inst/www of the package
#' @param dependency_script \code{character} A list of js files located in inst/www of the package
#' 
#' @export
#' @importFrom utils packageVersion
#' @details Taken from shinyWidgets package
attachstravavisDep <- function(tag, dependency="style.css", dependency_script = NULL) {
  version <- as.character(utils::packageVersion("stravachaser")[[1]])
  dep <- htmltools::htmlDependency(
    name = "stravachaser", version = version,
    package = "stravachaser",
    src = "inst/www", # - Fix upon loading without devtools
    stylesheet = dependency,
    script = dependency_script
  )
  message(paste0("ATTACHED '",dependency,"' from 'stravachaser' package."))
  
  if(!is.null(dependency_script)){
    message(paste0("ATTACHED '",dependency_script,"' from 'stravachaser' package."))
  }
  
  htmltools::attachDependencies(tag, dep, append = TRUE)
}