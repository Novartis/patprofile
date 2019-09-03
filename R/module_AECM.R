#' @param id A \code{character}. The module id. Must be consistent with the inputId
#'  in \code{callModule}.
#' @rdname aecm_mod
#' @export
aecm_UI <- function(id, title = "| AE & Conmeds |"){
  ns <- NS(id)
  tabPanel(title = title,
           selectInput(ns("aeColours"), "Colour AEs by:",
                       choices = c("Severity" = "severity",
                                   "Serious" = "serious",
                                   "Grade" = "grade",
                                   "Action taken" = "action"),
                       selected = "Severity"),
           uiOutput(ns("uio_AECM"))
  )
}

#' AECM Submodule
#'
#' Functions to enable the AE + Conmed submodule of the patient profile
#'
#' @param innput,output,session Shiny server arguments.
#' @inheritParams patientProfile_mod
#'
#' @rdname aecm_mod
#' @importFrom plotly renderPlotly plotlyOutput ggplotly layout
#' @export
aecm_mod <- function(input, output, session, uid, ADSL, ADAE, ADCM){
  output$uio_AECM <- renderUI({
    ns <- session$ns
    print("renderUI: AE + CM")
    aecm_list <- get_aecm_data(uid(), ADSL = ADSL(), ADAE = ADAE(), ADCM = ADCM())
    text <- paste("ID:", uid(), "by", input$aeColours)
    print(text)
    output$aecm_plotly <- renderPlotly({
      p2 <- plotly_aecm(aecm_list = aecm_list, aecolour = input$aeColours)
      p2$elementId <- NULL
      return(p2)
    })
    plotlyOutput(ns("aecm_plotly"), height = aecm_list$nrow * 22)
  })
}


plotly_aecm <- function(aecm_list, aecolour = "severity"){
  aecm <- aecm_list$aecm
  aecolour <- tolower(aecolour)
  if(aecolour == 'severity') aecm$colour = aecm$colour_sev
  if(aecolour == 'serious') aecm$colour = aecm$colour_ser
  if(aecolour == 'grade')    aecm$colour = aecm$colour_grade
  if(aecolour == 'action')   aecm$colour = aecm$colour_action

  if(aecolour == 'severity') {
    palette <- c('CM' = "#9318db", 'MILD' = "#FEB24C", 'MODERATE' = "#FC4E2A",
                 'SEVERE' = "#E31A1C", "NA" = "gray")
  } else if(aecolour == 'serious') {
    palette <- c('CM' = "#9318db", "Yes"= "#39b9ef", "No" = "#078bc4", "NA" = "gray")
  } else if (aecolour == 'grade') {
    palette <- c('CM' = "#9318db",
                 'NA' = "gray",
                 "Grade 0" = "#eff3ff",
                 "Grade 1" = "#bdd7e7",
                 "Grade 2" = "#6baed6",
                 "Grade 3" = "#3182bd",
                 "Grade 4" = "#08519c")
  } else if (aecolour == 'action') {
    palette <- c('CM' = "#9318db",
                 "DOSE NOT CHANGED" = "#66c2a5",
                 "DRUG INTERRUPTED" = "#fc8d62",
                 "DRUG WITHDRAWN" = "#8da0cb",
                 "DOSE REDUCED" = "#e78ac3",
                 "UNKNOWN" = "#a6d854")
  }
  dt <- data.table(aecm)
  dt[is.na(colour), colour := "NA"]
  dt[, text := paste0(name,
                      "<br>Start: ", format(start, format = "%d %b %Y"),
                      "<br>End: ", format(end, format = "%d %b %Y"),
                      "<br>Severity: ", colour_sev,
                      "<br>Serious: ", colour_ser,
                      "<br>Grade: ", colour_grade,
                      "<br>Action taken: ", colour_action
                      )]
  dt$id <- 1:nrow(dt)
  dtly <- melt(dt, measure.vars = c("start", "end"), value.name = "x")

  ply <-
    plot_ly(dtly, x = ~x, y = ~name, color = ~colour, colors = palette,
            type = "scatter", mode = "markers", text = ~text, hoverinfo = 'text') %>%
    add_lines(split = ~id, line = list(width = 7), showlegend = F) %>%
    layout(
      margin = list(l = 150),
      xaxis = list(title = ""), yaxis = list(title = ""),
      shapes = list(type = "rect", fillcolor = "green", opacity = 0.1,
                         layer = "below", line = list(width = 0),
                         x0 = aecm_list$aecm_ref[[1]], x1 = aecm_list$aecm_ref[[2]], xref = "x",
                         y0 = 0, y1 = length(unique(dtly$name))))
  return(ply)
}
