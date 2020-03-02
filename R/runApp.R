#!/usr/bin/Rscript

#' Launch SLAT GUI
#'
#' This function run the GUI shiny interface to run SLAT
#'
#' @examples
#' runSLAT()
runSLAT <- function() {
  appDir <- system.file("app", package = "SIAT")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }
  if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2) #pour éviter de surcharger les serveurs
  shiny::runApp(appDir = appDir,launch.browser = TRUE)
}
