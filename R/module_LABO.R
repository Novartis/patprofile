#' @rdname labo_mod
#' @export
labo_UI <- function(id){
  ns <- NS(id)
  tabPanel(title = "| Lab Overview |",
           br(),
           uiOutput(ns("uio_labo_col")),
           uiOutput(ns("uio_LABO")),
           br(),
           'Overview of lab results by Analysis Visit (unscheduled not displayed)'
  )
}

#' LABO Submodule
#'
#' Functions to enable the Lab overview submodule of the patient profile
#'
#' @param innput,output,session Shiny server arguments.
#' @inheritParams patientProfile_mod
#'
#' @rdname labo_mod
#' @export
labo_mod <- function(input, output, session, uid, ADLB){
  ns <- session$ns
  lab_reac <- reactiveVal()

  output$uio_labo_col <- renderUI({
    req(uid())
    # Get color choices
    lab_vals <- get_lab_values(uid = uid(), ADLB = ADLB())
    avail <- names(lab_vals)
    noshow <- c("USUBJID", "PARAM", "PARAMCD", "BASE", "VISIT", "VISITNUM",
                "AVISIT", "DTYPE", "AVAL", "ADT", "TRTSDT", "TRTEDT", "ANRHI",
                "ANRLO", "TRT01A", "ADY", "PARCAT1", "ONTRTFL", "ANRIND",
                "BNRIND", "ABLFL", "NORMAVAL", "date", "direction", "AVISIT2",
                "direction", "date")
    color_options <- avail[!avail %in% noshow]
    color_options <- c("None", sort(color_options))
    lab_col_selected <- ifelse("LBNRIND" %in% color_options, "LBNRIND", "None")
    lab_reac(lab_vals)
    tagList(
      selectInput(ns("labov_col"), label = "Color by",
                  choices = color_options, selected = lab_col_selected)
    )
  })
  output$uio_LABO <- renderUI({
    req(lab_reac)
    print("renderUI: Lab Overview")
    output$labov_plot <- renderPlot({
      p4 <- plot_lab_visits(lab_data = lab_reac(), color_by = input$labov_col)
      return(p4)
    })
    plotOutput(ns("labov_plot"), height = 600)
  })
}
