#' Start patient-profile app
#'
#' Start a simple app pre-loaded with all ADaM datasets.
#'
#' @importFrom shiny runApp
#' @export
showExamples <- function (example = 'safetyExpPatProfile', port = NULL, launch.browser = getOption("shiny.launch.browser", 
                                                                interactive()), host = getOption("shiny.host", "127.0.0.1"), 
          display.mode = c("auto", "normal", "showcase")) 
{
  examplesDir <- system.file("shiny-examples", package = "patprofile")
  dir <- shiny:::resolve(examplesDir, example)
  if (is.null(dir)) {
    if (is.na(example)) {
      errFun <- message
      errMsg <- ""
    }
    else {
      errFun <- stop
      errMsg <- paste("Example", example, "does not exist. ")
    }
    errFun(errMsg, "Valid examples are \"", paste(list.files(examplesDir), 
                                                  collapse = "\", \""), "\"")
  }
  else {
    runApp(dir, port = port, host = host, launch.browser = launch.browser, 
           display.mode = display.mode)

    setwd(dir)    
    
  }
}


