#' @rdname labp_mod
#' @export
labp_UI <- function(id){
  ns <- NS(id)
  tabPanel(title = "| Lab Parameters |",
           sidebarLayout(
             sidebarPanel(width = 3,
                          uiOutput(ns("uio_labp_param_selector"))
             ),
             mainPanel(
               br(),
               uiOutput(ns("uio_LABP"))
             ))
  )
}

#' LABP Submodule
#'
#' Functions to enable the lab parameters submodule of the patient profile.
#'
#' @param innput,output,session Shiny server arguments.
#' @inheritParams patientProfile_mod
#'
#' @rdname labp_mod
#' @export
labp_mod <-function(input, output, session, uid, ADLB){
  output$uio_labp_param_selector <- renderUI({
    req(uid())
    ns <- session$ns
    # Find LABP
    uADLB <- ADLB() %>% filter(!is.na(AVAL)) %>% distinct(PARAM, PARAMCD)
    lab_params <- setNames(uADLB$PARAMCD, uADLB$PARAM)
    lab_default <- intersect(c("ALT", "AST", "ALP", "BILI"), lab_params)
    # UI
    ns <- session$ns
    tagList(
      selectInput(inputId = ns("params_labp"), label = "Select parameters",
                  multiple = TRUE, choices = lab_params, selected = lab_default)
    )
  })
  output$uio_LABP <- renderUI({
    ns <- session$ns
    print("renderUI: Lab Parameters")
    # output$labpar_plot <- renderPlot({
    output$labpar_plot <- renderPlotly({
      p5 <- plotly_lab_param(uid = uid(),
                           params = input$params_labp,
                           ADLB = ADLB())
      p5$elementId <- NULL
      return(p5)
    })
    labp_height <- length(input$params_labp) * 200
    tagList(
      h4('Lab Plots for selected parameters'),
      # plotOutput(ns("labpar_plot"), height = labp_height, width = 900)
      plotlyOutput(ns("labpar_plot"), height = labp_height)#, width = 900)
    )
  })
}

#' @importFrom plotly plot_ly add_lines add_text layout subplot
plotly_lab_param <- function(uid, params, ADLB){
  selcols <- c("USUBJID", "PARAMCD", "BASE", "VISIT", "VISITNUM", "AVAL", "ADT",
               "ANRLO", "ANRHI", "TRTSDT", "TRTEDT", "PARAM", "ADY", "ONTRTFL")
  
  adlbcols <- names(ADLB)
  if ("A1LO" %in% names(ADLB))
    ADLB$ANRLO <- ADLB$A1LO
  
  if ("A1HI" %in% names(ADLB)) 
    ADLB$ANRHI <- ADLB$A1HI
  
  selADLB <- select(ADLB, one_of(selcols))
  dfLB <- selADLB %>%
    filter(USUBJID == uid, PARAMCD %in% params, !is.na(AVAL)) %>%
    mutate(date = as.Date(ADT, origin = '1960-01-01'))

  dfLB <- dfLB %>%
    group_by(PARAMCD) %>%
    mutate(range_l = min(as.numeric(ANRLO), na.rm=TRUE),
           range_h = max(as.numeric(ANRHI), na.rm=TRUE))


  miny <- 0.97 * (min(min(dfLB$AVAL, na.rm=TRUE), min(dfLB$range_l, na.rm=TRUE)))
  maxy <- 1.03 * (max(max(dfLB$AVAL, na.rm=TRUE), min(dfLB$range_h, na.rm=TRUE)))

  if("TRTSDT" %in% names(dfLB)){
    minx <- as.Date(min(dfLB$ADT, dfLB$TRTSDT - 30),  origin = '1960-01-01')
  } else{
    minx <- as.Date(min(dfLB$ADT),  origin = '1960-01-01')
  }
  if("TRTEDT" %in% names(dfLB)){
    maxx <- as.Date(max(dfLB$ADT, dfLB$TRTEDT + 30),  origin = '1960-01-01')
  } else{
    maxx <- as.Date(max(dfLB$ADT),  origin = '1960-01-01')
  }
  ylab <- unique(dfLB$PARAM)

  # 2 Plot with facets
  dfLB <- dfLB %>% arrange(PARAMCD, date)
  plots <- lapply(params, function(param){
    plot_ly(data = dfLB %>% filter(PARAMCD == param), x = ~date, y = ~AVAL, #color = ~PARAM,
            hoverinfo = "text",
            text = ~paste0(PARAM, "<br>Value: ", AVAL, '<br>Date: ', date),
            type = 'scatter', mode = 'lines+markers') %>%
      # UNL & LNL
      add_lines(y = ~range_h, color = I("red"), text = ~paste("UNL: ", range_h),
                hoverinfo = "text", line = list(dash = 'dot')) %>%
      add_text(x = minx, y = ~range_h, textposition = "bottom right", text = "UNL",
               hoverinfo='skip', color = I("red")) %>%
      add_lines(y = ~range_l, color = I("red"), text = ~paste("LNL: ", range_l),
                hoverinfo = "text", line = list(dash = 'dot')) %>%
      add_text(x = minx, y = ~range_l, textposition = "top right", text = "LNL",
               hoverinfo='skip', color = I("red")) %>%
      layout(yaxis = list(title = ~PARAMCD))
  })
  labply <-
    subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE, titleY = TRUE) %>%
    layout(showlegend = FALSE)

  return(labply)
}
