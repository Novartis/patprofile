#' @rdname ecg_mod
#' @export
ecg_UI <- function(id){
  ns <- NS(id)
  tabPanel(title = "| ECG |",
           sidebarLayout(
             sidebarPanel(width = 3,
                          uiOutput(ns("uio_ecg_param_selector"))
             ),
             mainPanel(
               uiOutput(ns("uio_ECG"))
             )
           )
  )
}

#' ECG Submodule
#'
#' Functions to enable the electrocardiogram (ECG) submodule of the patient profile.
#'
#' @param innput,output,session Shiny server arguments.
#' @inheritParams patientProfile_mod
#'
#' @rdname ecg_mod
#' @export
ecg_mod <- function(input, output, session, uid, ADEG){
  ecg_choices <- reactive({
    ecg_list <- list()
    # ecg_allowed <- c("QTCF", "QRSDUR", "QTMEAN", "PRMEAN", "HRMEAN", "RRMEAN")
    ecg_param_tab <- ADEG() %>%
      filter(USUBJID == uid() & !is.na(AVAL)) %>% # & PARAMCD %in% ecg_allowed) %>%
      distinct(PARAMCD, PARAM)

    print(ecg_param_tab)
    ecg_list$ecg_params <- ecg_param_tab
    return(ecg_list = ecg_list)
  })
  output$uio_ecg_param_selector <- renderUI({
    ns <- session$ns

    tagList(
      selectInput(inputId = ns("param_ecg"), label = "Select parameters",
                  choices = ecg_choices()$ecg_params$PARAM, selected = ecg_choices()$ecg_params$PARAM[1])
                  # choices = ecg_choices_tab$PARAM, selected = ecg_choice_tab$PARAM[1])
    )
  })
  output$uio_ECG <- renderUI({
    ns <- session$ns
    print("renderUI: ECG")
    ecgpar <- input$param_ecg
    paramcd <- ecg_choices()$ecg_params %>% filter(PARAM == ecgpar)
    paramcd <- paramcd$PARAMCD
    output$ecg_plot <- renderPlot({
      p7 <- plot_ECG(uid = uid(), paramcd = paramcd,#input$param_ecg,
                     ADEG = ADEG())[["plot"]]
      return(p7)
    })
    plotOutput(ns("ecg_plot"))
  })
}

