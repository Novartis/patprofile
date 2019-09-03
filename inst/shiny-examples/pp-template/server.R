function(input, output, session) {
  myADSL <- reactive({
    adslf <- system.file("data/ADSL.sas7bdat", package = "pprofile")
    haven::read_sas(adslf)
  })
  myADCM <- reactive({
    adcmf <- system.file("data/ADCM.sas7bdat", package = "pprofile")
    haven::read_sas(adcmf)
  })
  myADAE <- reactive({
    adaef <- system.file("data/ADAE.sas7bdat", package = "pprofile")
    haven::read_sas(adaef)
  })
  myADLB <- reactive({
    adlbf <- system.file("data/ADLB.sas7bdat", package = "pprofile")
    haven::read_sas(adlbf)
  })
  myADPC <- reactive({
    adpcf <- system.file("data/ADPC.sas7bdat", package = "pprofile")
    haven::read_sas(adpcf)
  })
  myADEG <- reactive({
    adegf <- system.file("data/ADEG.sas7bdat", package = "pprofile")
    haven::read_sas(adegf)
  })
  myADEX <- reactive({
    adexf <- system.file("data/ADEX.sas7bdat", package = "pprofile")
    haven::read_sas(adexf)
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
    req(myADSL(), myADAE(), myADCM(), myADLB(),
        myADEG(), myADEX(), myADPC())
    callModule(patientProfile_mod, id = "pp_module1", #uid = uid,
               ADSL = myADSL, ADAE = myADAE, ADCM = myADCM, ADLB = myADLB,
               ADEG = myADEG, ADEX = myADEX, ADPC = myADPC)
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

