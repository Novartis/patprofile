#' @rdname overview_mod
#' @export
overview_modUI <- function(id){
  ns <- NS(id)
  tabPanel(
    title = "| Overview |",
    br(),
    uiOutput(ns("uio_tables"))
  )
}

overview_mod2 <- function(input, output, session, uid, ADSL, ADLB){
  ns <- session$ns
  lapply(1:2, function(i){
    output[[paste0("table", i)]] <- renderText({paste0("text", i)})
  })
  output$uio_tables <- renderUI({
    list(textOutput(ns("table1")),
         textOutput(ns("table2")))
  })
}


overview_mod0 <- function(input, output, session,
                         uid, ADSL){#, ADAE = NULL, ADCM = NULL,
                         # ADLB = NULL, ADEG = NULL, ADPC = NULL, ADEX = NULL){
  ns <- session$ns
  # Collect all available tables
  tables <- reactiveValues()
  # Add tabkes to the table list
  observe({
    req(ADSL, uid())
    print(paste0("UID: ",uid()))
    uADSL <- ADSL() %>% filter(USUBJID == uid()) %>% select(1:10)
    tables$ADSL <- uADSL[1,]
  })
  observe({
    req(uid())
    print("renderUI: Overview")
    output$adsl_oview <- renderDataTable(expr = {
      tables$ADSL
    }, options = list(pageLength = 1, dom = 't'))
    output$table_name <- renderText({"ADSL"})
  })
  output$uio_tables <- renderUI({
    tagList(
      textOutput(ns("table_name")),
      br(),
      dataTableOutput(ns("adsl_oview"))
    )
  })
}


#TODO: wrap table loaders into a function
# usage: ADaM$ADPC <- .add_table(uid, ADPC, nrow = 5)
.add_table <- function(session, uid, reac_table, nrow = 10){
  req(!is.null(reac_table), !is.null(reac_table(), uid())) #Order matters test reac exist before test function return
  u_table <- reac_table() %>% filter(USUBJID == uid()) %>% select(1:10)
  return(u_table)
}


#' Overview Submodule
#'
#' Functions to enable the overview submodule of the patient profile
#'
#' @param innput,output,session Shiny server arguments.
#' @inheritParams patientProfile_mod
#'
#' @details
#' Only available tables will be displayed. By default, the first ten columns
#' are shown. More customization will be added later.
#'
#' @rdname overview_mod
#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#' @export
overview_mod <- function(input, output, session, uid, ADSL = NULL,
                         ADLB = NULL, ADAE = NULL, ADCM = NULL,
                         ADEG = NULL, ADPC = NULL, ADEX = NULL){
  ns <- session$ns
  # Collect all available tables
  ADaM <- reactiveValues()
  # Add tabkes to the table list
  observe({
    req(!is.null(ADSL), !is.null(ADSL()), uid())
    uADSL <- ADSL() %>% filter(USUBJID == uid()) %>% select(1:10)
    ADaM$ADSL <- uADSL[1,]
  })
  observe({
    req(!is.null(ADLB), !is.null(ADLB()), uid())
    uADLB <- ADLB() %>% filter(USUBJID == uid()) %>% select(1:10)
    ADaM$ADLB <- uADLB[1:10,]
  })
  observe({
    req(!is.null(ADAE), !is.null(ADAE()), uid())
    uADAE <- ADAE() %>% filter(USUBJID == uid()) %>% select(1:10)
    ADaM$ADAE <- uADAE[1:10,]
  })
  observe({
    req(!is.null(ADCM), !is.null(ADCM()), uid())
    uADCM <- ADCM() %>% filter(USUBJID == uid()) %>% select(1:10)
    ADaM$ADCM <- uADCM[1:10,]
  })
  observe({
    req(!is.null(ADEG), !is.null(ADEG()), uid())
    uADEG <- ADEG() %>% filter(USUBJID == uid()) %>% select(1:10)
    ADaM$ADEG <- uADEG[1:10,]
  })
  observe({
    req(!is.null(ADEX), !is.null(ADEX()), uid())
    uADEX <- ADEX() %>% filter(USUBJID == uid()) %>% select(1:10)
    ADaM$ADEX <- uADEX[1:10,]
  })
  observe({
    req(!is.null(ADPC), !is.null(ADPC()), uid())
    uADPC <- ADPC() %>% filter(USUBJID == uid()) %>% select(1:10)
    ADaM$ADPC <- uADPC[1:10,]
  })

  output$uio_tables <- renderUI({
    tbl <- reactiveValuesToList(ADaM)
    print(names(tbl))
    tbl <- rev(tbl)#TODO: Make a default order
    print(paste("renderUI: Overview,", length(tbl), "tables"))

    lapply(seq_along(tbl), function(i){
      ns_nmi <- paste0("name", i)
      ns_tbi <- paste0("table", i)
      output[[ns_nmi]] <- renderText({names(tbl)[[i]]})
      output[[ns_tbi]] <- renderDataTable({tbl[[i]]},
                            options = list(pageLength = 5, dom = 't'))
    })

    lapply(seq_along(tbl), function(i) { # This CANNOT be a for loop
      nm_id <- paste0("name", i)
      tb_id <- paste0("table", i)
      list(
        textOutput(ns(nm_id)),
        dataTableOutput(ns(tb_id))
      )
    })
  })
}
