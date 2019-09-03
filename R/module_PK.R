#' @rdname pk_mod
#' @export
pk_UI <- function(id){
  ns <- NS(id)
  tabPanel(title = "| PK |",
           sidebarLayout(
             sidebarPanel(width = 3,
                          uiOutput(ns("uio_pk_param_selector"))
             ),
             mainPanel(
               uiOutput(ns("uio_pk_plot"))
             )
           )
  )
}

#' PK Submodule
#'
#' Functions to enable the pharmacokinetics (PK) submodule of the patient profile.
#'
#' @param innput,output,session Shiny server arguments.
#' @inheritParams patientProfile_mod
#'
#' @rdname pk_mod
#' @export
pk_mod <- function(input, output, session, uid, ADPC){
  output$uio_pk_param_selector <- renderUI({
    ns <- session$ns
    pk_choices <- sort(unique(ADPC()$PARAM))
    pk_choices <- pk_choices[pk_choices != ""]
    tagList(
      selectInput(inputId = ns("param_pk"), label = "Select parameters",
                  choices = pk_choices, selected = pk_choices[1])
    )
  })
  output$uio_pk_plot <- renderUI({
    ns <- session$ns
    print("renderUI: PK")
    output$pk_plot <- renderPlot({
      p8 <- plot_PK(uid = uid(), param = input$param_pk,
                    ADPC = ADPC())[["plot"]]
      return(p8)
    })
    plotOutput(ns("pk_plot"))
  })
}
