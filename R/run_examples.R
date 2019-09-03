#' Start patient-profile app
#'
#' Start a simple app pre-loaded with all ADaM datasets.
#'
#' @importFrom shiny runApp
#' @export
run_template <- function(){
  appdir <- system.file("shiny-examples/pp-template/", package = "pprofile")
  print(paste("App in", appdir))
  runApp(appDir = appdir)
}
