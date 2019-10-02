#' @param id A \code{character}. The namespace for the module.
#' @rdname patientProfile_mod
#' @export
patientProfile_modUI <- function(id){
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
   # tabPanel(
    #  title = "Patient profiles",
      wellPanel(
        sidebarLayout(
          sidebarPanel(width = 3, align = "center",
                       uiOutput(ns("uio_patient_selector"))
          ),
          mainPanel(
            uiOutput(ns("uio_patient_summ"))
          )
        )
      ),
      tabsetPanel(
        id = ns("main_tabset"),
        overview_modUI(ns("overview_tab"))
      )
    #)
  )
}
