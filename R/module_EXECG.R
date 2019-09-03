#' @rdname execg_mod
#' @export
execg_UI <- function(id){
  ns <- NS(id)
  tabPanel(title = "| Exposure & ECG |",
           sidebarLayout(
             sidebarPanel(width = 3,
                          uiOutput(ns("uio_exeg_param_selector"))
             ),
             mainPanel(
               uiOutput(ns("uio_EXEG"))
             )
           )
  )
}

#' EXECG Submodule
#'
#' Functions to enable the exposure (EX) & electrocardiogram (ECG) submodule of the
#' patient profile.
#'
#' @param innput,output,session Shiny server arguments.
#' @inheritParams patientProfile_mod
#'
#' @rdname execg_mod
#' @export
execg_mod <- function(input, output, session, uid, ADSL, ADEX, ADPC, ADEG){
  output$uio_exeg_param_selector <- renderUI({
    ns <- session$ns
    ecg_choices <- c("QTCF", "QRSDUR", "QTMEAN", "PRMEAN", "HRMEAN", "RRMEAN")
    tagList(
      selectInput(inputId = ns("param_execg"), label = "Select parameters",
                  choices = ecg_choices, selected = "QTCF")
    )
  })
  output$uio_EXEG <- renderUI({
    ns <- session$ns
    input$patient_selector
    print("renderUI: Exposure & ECG")
    output$exeg_plot <- renderPlot({
      p6 <- plot_EXEG(uid = uid(), param = input$param_execg,
                      ADSL = ADSL(), ADEX = ADEX(), ADPC = ADPC(), ADEG = ADEG())
      return(p6)
    })
    plotOutput(ns("exeg_plot"), height=600, width=1600)
  })
}
