function(input, output, session) {
  myADSL <- reactive({
    haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/adsl.xpt")
  })
  myADCM <- reactive({
    haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/adcm.xpt")
  })
  myADAE <- reactive({
    haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/adae.xpt")
  })
  myADLB <- reactive({
    haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/adlbc.xpt")
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
  output$linked_ply <- renderPlotly({
    req(myADLB())
    ns <- session$ns
    data <- filter(myADLB(), PARAMCD == "ALT")
    p <- plot_ly(data = data, x = ~ADT, y = ~AVAL,
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
}

