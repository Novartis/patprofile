#' Patient profile module
#'
#' Server and UI functions of the patient profile module
#'
#' @param innput,output,session Shiny server arguments.
#' @param uid A \code{reactive} \code{character}. A unique subject identifier.
#' @param ADSL A \code{reactive} \code{data.frame} of ADaM formatted Subject Level data.
#'  This cannot be \code{NULL}.
#' @param ADAE A \code{reactive} \code{data.frame} of ADaM formatted Adverse Event data.
#' @param ADCM A \code{reactive} \code{data.frame} of ADaM formatted Concomitant Medicine data.
#' @param ADLB A \code{reactive} \code{data.frame} of ADaM formatted Laboratory analysis data.
#' @param ADEG A \code{reactive} \code{data.frame} of ADaM formatted Electrocardiogram data.
#' @param ADEX A \code{reactive} \code{data.frame} of ADaM formatted EXposure data.
#' @param ADPC A \code{reactive} \code{data.frame} of ADaM formatted Pharmacokinetics data.
#'
#' @details
#' Aside from \code{ADSL}, all datasets are optional. Visualization for
#' missing datasets will be missing. See submodules documentation for
#' data requirements of each tab of the participant profile module.
#'
#' @seealso \code{aecm_mod}, \code{exlb_mod}, \code{labo_mod}, \code{labp_mod},
#'  \code{execg_mod}, \code{ecg_mod}, \code{pk_mod}
#'
#' @rdname patientProfile_mod
#' @export
patientProfile_mod <- function(input, output, session, uid = NULL,
                               ADSL = NULL, ADAE = NULL, ADCM = NULL,
                               ADLB = NULL, ADEG = NULL, ADPC = NULL, ADEX = NULL){

  uid <- reactiveVal()
  observe({uid(input$patient_selector)}) #Always write current patient to uid
  ns <- session$ns
  profile_dat <- reactiveValues() # reactiveValues are visible within the module server
  observe({
    req(ADSL())
    profile_dat$ADSL <- as.data.frame(ADSL())
    profile_dat$patient_list <- unique(profile_dat$ADSL$USUBJID)
  })

  # List patients
  output$uio_patient_selector <- renderUI({
    tagList(
      selectInput(inputId = ns("patient_selector"), label = "Select patient", choices = profile_dat$patient_list),
      actionButton(inputId = ns("prev_p"), label = "", icon = icon('angle-double-left')),
      actionButton(inputId = ns("next_p"), label = "", icon = icon('angle-double-right')),
      # TODO: Add download report for the patient
      # downloadButton(ns("dwn")),
      br()
    )
  })
  observeEvent(input$prev_p, {
    list_patients <- profile_dat$patient_list
    oldPatient <- input$patient_selector
    old_i <- which(list_patients == oldPatient)
    if(old_i > 1){
      new_i <- old_i-1
      newPatient <- list_patients[new_i]
      updateSelectInput(session = session, inputId = "patient_selector", selected = newPatient)
    }
  })
  observeEvent(input$next_p, {
    list_patients <- profile_dat$patient_list
    oldPatient <- input$patient_selector
    old_i <- which(list_patients == oldPatient)
    if(old_i < length(list_patients)){
      new_i <- old_i+1
      newPatient <- list_patients[new_i]
      updateSelectInput(session = session, inputId = "patient_selector", selected = newPatient)
    }
  })

  # Grab events from shiny.onInputChange
  observe({
    ns <- session$ns
    if(!is.null(input$patient_js)){
      print("Updating patient selection in module")
      updateSelectInput(session = session, inputId = "patient_selector",
                        selected = input$patient_js)}
    }
  )

  # Patient summary
  output$uio_patient_summ <- renderUI({
    print(paste("renderUI: Patient summary", input$patient_selector))
    selected_ADSL <- profile_dat$ADSL
    selected_ADSL <- selected_ADSL[selected_ADSL$USUBJID == input$patient_selector,]
    selected_ADSL <- selected_ADSL[1,] #For studies with multiple entries
    patient_summary_txt <- .get_summary_text(selected_ADSL)
    output$patient_summary <- renderText(patient_summary_txt)
    tagList(
      HTML(paste0(tags$span(style="color:red", "This module is not validated. It is for exploratory analysis only."))),
      htmlOutput(ns("patient_summary"))
    )
  })

  #output$uio_main <- renderUI({
  #  ns <- session$ns
  #  tagList(
  #    tabsetPanel(
  #      id = ns("main_tabset"),
  #      overview_modUI(ns("overview_tab"))
  #    )
  #  )
  #})

  # TAB1: Tabular Overview (Flex)
  callModule(module = overview_mod, id = "overview_tab", uid = uid, ADSL = ADSL,
             ADLB = ADLB, ADAE = ADAE, ADCM = ADCM,
             ADEG = ADEG, ADPC = ADPC, ADEX = ADEX)
  # TAB2: Concomitant Medicines (SL, AE, CM)
  observe({
    req(ADSL, ADAE, ADCM,
        !is.null(ADSL()),  !is.null(ADAE()), !is.null(ADCM()))
    callModule(module = aecm_mod, id = "aecm_auto", uid = uid,
               ADSL = ADSL, ADAE = ADAE, ADCM = ADCM)
    appendTab(inputId = "main_tabset", aecm_UI(ns("aecm_auto")))
  })
  # TAB3: Exposures and Lab values
  observe({
    req(ADSL, ADLB, ADEX, ADPC,
        !is.null(ADSL()), !is.null(ADLB()), !is.null(ADEX()), !is.null(ADPC()))
    callModule(module = exlb_mod, id = "exlb_auto", uid = uid,
               ADSL = ADSL, ADLB = ADLB, ADEX = ADEX, ADPC = ADPC)
    appendTab(inputId = "main_tabset", exlb_UI(ns("exlb_auto")))
  })
  # TAB4: Lab Overview (LB)
  observe({
    req(ADLB, !is.null(ADLB()))
    ns <- session$ns
    callModule(module = labo_mod, id = "labo_auto", uid = uid, ADLB = ADLB)
    appendTab(inputId = "main_tabset", labo_UI(ns("labo_auto")))
  })
  # TAB5: Lab Parameters (ADLB)
  observe({
    req(ADLB, !is.null(ADLB()))
    callModule(module = labp_mod, id = "labp_auto", uid = uid, ADLB = ADLB)
    appendTab(inputId = "main_tabset", labp_UI(ns("labp_auto")))
  })
  # TAB6: Exposure & ECG
  observe({
    req(ADSL, ADEX, ADEG, ADPC,
        !is.null(ADSL()), !is.null(ADEX()), !is.null(ADEG()), !is.null(ADPC()))
    callModule(module = execg_mod, id = "execg_auto", uid = uid,
               ADSL = ADSL, ADEX = ADEX, ADPC = ADPC, ADEG = ADEG)
    appendTab(inputId = "main_tabset", execg_UI(ns("execg_auto")))
  })
  # TAB7: ECG (ADEG)
  observe({
    req(ADEG, !is.null(ADEG()))
    callModule(module = ecg_mod, id = "ecg_auto", uid = uid, ADEG = ADEG)
    appendTab(inputId = "main_tabset", ecg_UI(ns("ecg_auto")))
  })
  # TAB8: PK (ADPC)
  observe({
    req(ADPC, !is.null(ADPC()))
    callModule(module = pk_mod, id = "pk_auto", uid = uid, ADPC = ADPC)
    appendTab(inputId = "main_tabset", pk_UI(ns("pk_auto")))
  })
  return("RETURN")
}
