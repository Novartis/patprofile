
jsCode_edish <-  "
function(el, rSettings) {
el.chart.chart.wrap.on('participantsSelected', function(chart) {
console.log('Participant Selected!! You clicked participant: '+chart.participantsSelected)
Shiny.setInputValue('pp_module1-patient_js', chart.participantsSelected[0]); 
});
}
"

jsCode_other <- "
function(el, x) {
el.chart.wrap.on('participantsSelected', function(chart) {
console.log('Participant Selected!! You clicked participant: '+chart.participantsSelected)
Shiny.setInputValue('pp_module1-patient_js', chart.participantsSelected[0]); 
});
}
"

function(input, output, session) {
  myADSL <- reactive({
    haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/adsl.xpt")
  })
  myADCM <- reactive({
    cm <- haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/adcm.xpt")
    cm$CMDECOD <- cm$CMTRT
    cm
  })
  myADAE <- reactive({
    haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/adae.xpt")
  })
  myADLB <- reactive({
    lb <- haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/adlbc.xpt")
    #subjs <- lb %>% pull (USUBJID) %>% unique() %>% sample(80)
    #lb1 <- lb %>% filter(USUBJID %in% subjs)
    lb
  })
  
  myADPC <- reactive({
    ADPC <- haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/adpc.xpt")
    
    adpcCols <- names(ADPC)
    
    if (! ("AVISIT" %in% adpcCols | "AVISITN" %in% adpcCols) & "VISITNUM" %in% adpcCols) {
      ADPC$AVISIT <- factor(ADPC$VISITNUM)
      ADPC$AVISITN <- ADPC$VISITNUM
    }
    
    ADPC %>% select(-USUBJID) %>% mutate( SUBJID = substring(SUBJID, first=4)) %>% left_join( myADSL() %>% select(SUBJID, USUBJID), by="SUBJID" )
    
  })
  # Grab events from shiny.onInputChange
  uid <- reactiveVal()
  observe({
    if(!is.null(input$patient_js)){
      print("Updating selected patient_js1")
      uid(input$patient_js)
    }
  })
  
  # Main module call
  observe({
    req(myADSL(), myADAE(), myADCM(), myADLB(), myADPC())
    callModule(patientProfile_mod, id = "pp_module1", #uid = uid,
               ADSL = myADSL, ADAE = myADAE, ADCM = myADCM, ADLB = myADLB, ADPC = myADPC)
  })
  # Linked plot
  output$linked_ply <- renderPlotly( {
    req(myADLB())
    ns <- session$ns
    
    heplab <- myADLB() %>% 
      filter ( PARAMCD %in% c("ALT", "BILI") ) %>% 
      group_by(USUBJID, PARAMCD) %>%
      summarize( max = max(AVAL)) %>% 
      dplyr::ungroup() %>%
      pivot_wider( names_from = PARAMCD, values_from = max)
    
    
    p <- plot_ly(data = heplab, x = ~ALT, y = ~BILI,
                 key = ~USUBJID,
                 type = "scatter", mode = "markers",
                 text = ~paste("uid:", USUBJID))
    p$elementId <- NULL # Suppress warnings (plotly #985)
    print(paste("Input binding:", ns("patient_js")))
    p <- p %>% onRender("
      function(el){
        el.on('plotly_click', function(d){
          selsub = d.points[0].data.key[d.points[0].pointNumber];
          console.log('d object= ', d);
          console.log('Click: ', selsub);
          console.log('id: ', d.points[0].pointNumber);
          Shiny.onInputChange('pp_module1-patient_js', selsub);
        })
      }
    ")
    return(p)
  })
  output$click <- renderText({
    d <- event_data("plotly_click")
    if(is.null(d)){
      "Click events appear here (double-click to clear)"
    } else{
      paste0("Clicked ", d$key, ". Go back to the patient profile tab to get more information about this patient.")
    }
  })
  
  output$aeexp <- renderAeExplorer({
    aevars <- c("USUBJID","AEBODSYS", "AEDECOD", "TRTA" , "AESER", "AESEV","AEREL", "AEOUT")
    aeExplorer(myADAE() %>% select( one_of(aevars)), group_col = "TRTA")
  })
  
  output$aetim <- renderAeTimelines({
    aevars <- c("USUBJID","AEBODSYS", "AEDECOD", "TRTA" ,"AESEQ", "AESER", "AESEV","AEREL", "AEOUT", "ASTDY", "AENDY")
    aeTimelines(as.data.frame(myADAE()%>% select( one_of(aevars)) %>% filter(ASTDY > -10) )) %>%
      onRender(jsCode = jsCode_other)
  })
  
  output$outexp <- renderSafetyOutlierExplorer({
    safetyOutlierExplorer(myADLB(), time_col = c("AVISITN","ADY"), time_order_col = c("AVISITN", "null"),
                          measure_col = "PARAM", value_col="AVAL", normal_col_low = "A1LO", normal_col_high = "A1HI"
    ) %>%
      onRender(jsCode = jsCode_other)
  })
  
  output$edish <- renderEDISH({
    
    
    lbvars <- c("USUBJID", "AVAL", "PARAM", "AVISIT", "AVISITN", "ADY", "A1LO", "A1HI", "TRTA")
    eDISH(data=myADLB() %>% 
            #filter(USUBJID %in% subjs) %>% 
            select( one_of(lbvars) ), 
          id_col = "USUBJID",
          value_col = "AVAL", 
          measure_col = "PARAM", 
          visit_col = "AVISIT",
          visitn_col = "AVISITN", 
          studyday_col = "ADY",
          normal_col_low = "A1LO", 
          normal_col_high = "A1HI", 
          group_cols = list(
            list(value_col = "TRTA", label = "Treatment"),
            list(value_col = "SEX", label = "Sex"), 
            list(value_col = "AGEGR1", label = "Age group")
          ),
          measure_values = list(ALT = "Alanine Aminotransferase (U/L)",
                                AST = "Aspartate Aminotransferase (U/L)",
                                TB = "Bilirubin (umol/L)",
                                ALP = "Alkaline Phosphatase (U/L)")) %>%
      onRender(jsCode = jsCode_edish)
    
  })
  
  output$shiftplot <- renderSafetyShiftPlot({
    safetyShiftPlot( myADLB(),  
                     time_col = c("AVISITN"), 
                     measure_col = "PARAM", 
                     value_col="AVAL" ) %>%
      onRender(jsCode = jsCode_other)
  })
  
  output$histplot <- renderSafetyHistogram({
    safetyHistogram( myADLB(),
                     measure_col = "PARAM", 
                     value_col="AVAL",
                     normal_col_low = "A1LO", 
                     normal_col_high = "A1HI")
  })
  
  output$boxplot <- renderSafetyResultsOverTime({
    safetyResultsOverTime(
      myADLB(),
      time_col = c("AVISIT"), time_order_col = c("AVISITN"),
      measure_col = "PARAM", value_col="AVAL", 
      normal_col_low = "A1LO", normal_col_high = "A1HI",
      groups_col = "TRTA"
    )
  })
  
}

