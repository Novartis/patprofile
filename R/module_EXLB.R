#' @rdname exlb_mod
#' @export
exlb_UI <- function(id){
  ns <- NS(id)
  tabPanel(title = "| Exposure & Labs |",
           sidebarLayout(
             sidebarPanel(width = 2,
                          checkboxGroupInput(ns('exlb_params'), 'Lab parameters to display',
                                             choices = c('ALP', 'ALT', 'AST', 'BILI'),
                                             selected= c('ALP', 'ALT', 'AST', 'BILI'))
             ),
             mainPanel(
               h4('Lab parameters normalized to the ULN'),
               br(),
               uiOutput(ns("uio_EXLB"))
             ))
  )
}

#' EXLB Submodule
#'
#' Functions to enable the exposure (EX) and lab data (LB) submodule of the patient profile
#'
#' @param innput,output,session Shiny server arguments.
#' @inheritParams patientProfile_mod
#'
#' @rdname exlb_mod
#' @export
exlb_mod <- function(input, output, session, uid, ADSL, ADLB, ADEX, ADPC){
  output$uio_EXLB <- renderUI({
    ns <- session$ns
    req(uid())
    print("renderUI: EX + LB")
    output$exlb_plot <- renderPlot({
      p3 <- plot_EXLB(uid = uid(), ADLB = ADLB(), ADSL = ADSL(),
                      ADEX = ADEX(), ADPC = ADPC(), paramcd = input$exlb_params)
      return(p3)
    })
    plotOutput(ns("exlb_plot"))
  })
}
