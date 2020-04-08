
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
plot.Tendril2 <- function(x, ...) {
  
  params <- as.list(substitute(list(...)))
  
  if (is.null(params$coloring)) {
    params$coloring = "Terms"
  }
  
  if (is.null(params$interactive)) {
    params$interactive = FALSE
  }
  
  if (!params$interactive) {
    p <- ggplot2_plotbasic(x, coloring=params$coloring)
  } else {
    p <- plotly_plotbasic2(x, coloring=params$coloring)
  }
  
  return(p)
}

plotly_plotbasic2 <- function(tendril, coloring, opacity=0.5) {
  `%>%` <- magrittr::`%>%`
  cc= tendril$data[[coloring]]
  
  # if(coloring %in% c("p", "p.adj", "fish")) {
  #   cc <- -log10(cc)
  #   cc[cc<(-3)] <- -3
  # }
  
  palette <- Tendril:::tendril_palette()
  max_termscount <- max(tendril$data$TermsCount, na.rm = TRUE)
  p <- tendril$data %>%
    dplyr::group_by(Terms) %>%
    plotly::plot_ly(x=~x, y=~y, width = 700, height = 700)%>%
    plotly::add_markers(              #mode = "markers", type = "scatter",
      size=~(TermsCount/max_termscount)*10, opacity=opacity, sizes=c(30,50),
      symbol = ~Treat,
      color = as.formula(paste0('~', coloring)),
      colors = palette$grpalette,
      #line = list(color = "lightgrey"),
      #text = ~paste("subjid = ",Unique.Subject.Identifier,"<br>Term: ", Terms,"<br> Arm:",Treat, '<br>Start day:', StartDay, '<br>p.adjusted:', round(p.adj, 4)),
      text = ~paste(
        "subjid = ",Unique.Subject.Identifier,
        "<br>Term: ", Terms,
        "<br>Arm:",  Treat,
        '<br>Start day:', StartDay,
        '<br>Frequency:', TermsCount,
        '<br>p.adjusted:', round(p.adj, 4),
        sep=""), 
      hoverinfo = "text",
      customdata = ~Unique.Subject.Identifier) %>%
    plotly::add_trace(color=I("lightgrey"), mode="lines", type="scatter", opacity=opacity, showlegend = FALSE)%>%
    plotly::add_annotations(
      x = 0,
      y = 1,
      xref = "paper",
      yref = "paper",
      text = tendril$Treatments[2],
      xanchor = "left",
      showarrow = F
    ) %>%
    plotly::add_annotations(
      x = 1,
      y = 1,
      xref = "paper",
      yref = "paper",
      text = tendril$Treatments[1],
      xanchor = "right",
      showarrow = F
    ) %>%
    plotly::layout(xaxis = list(nticks = 10, showticklabels = FALSE, title = ""),
                   yaxis = list(scaleanchor = "x", showticklabels = FALSE, title = "")
    )
  
  return(p)
}

function(input, output, session) {
  
  
  myADSL0 <- reactive({
    haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/adsl.xpt")
  })
  
  myADPC <- reactive({
    ADPC <- haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/adpc.xpt")
    
    adpcCols <- names(ADPC)
    
    if (! ("AVISIT" %in% adpcCols | "AVISITN" %in% adpcCols) & "VISITNUM" %in% adpcCols) {
      ADPC$AVISIT <- factor(ADPC$VISITNUM)
      ADPC$AVISITN <- ADPC$VISITNUM
    }
    
    ADPC %>% select(-USUBJID) %>% mutate( SUBJID = substring(SUBJID, first=4)) %>% left_join( myADSL0() %>% select(SUBJID, USUBJID), by="SUBJID" )
    
  })
  
  subjs <- reactive({
    unique( c(myADPC() %>% pull (USUBJID) %>% unique() ,"01-705-1186", myADSL0() %>% pull(USUBJID) ))#%>% sample(40)) )
  })
  
  myADSL <- reactive({
    myADSL0() %>% filter(USUBJID %in% subjs())
  })
  myADCM <- reactive({
    cm <- haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/adcm.xpt")
    cm$CMDECOD <- cm$CMTRT
    cm %>% filter(USUBJID %in% subjs())
  })
  myADAE <- reactive({
    haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/adae.xpt") %>% filter(USUBJID %in% subjs())
  })
  myADLB <- reactive({
    lb <- haven::read_xpt("https://github.com/phuse-org/phuse-scripts/raw/master/data/adam/cdisc/adlbc.xpt")
    lb %>% filter(USUBJID %in% subjs())
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

  
  # Tendril plot
  # Linked plot
  output$tendrilTab <- renderUI({
    req(myADAE())
    
    tagList(
      sidebarLayout(
        sidebarPanel(
          width = 2,
          selectizeInput("armSelector1", label = "First Arm", choices = unique(myADAE()$TRTA), selected = "Placebo"),
          selectizeInput("armSelector2", label = "Second Arm", choices = unique(myADAE()$TRTA), selected = "Xanomeline High Dose")
        ),
        mainPanel(
          width = 10,
          verbatimTextOutput("tendrilSelectedID"),
          plotlyOutput("tendrilPlot")
        )
      )
    )
  })
  
  output$tendrilSelectedID <- renderPrint({
    req(input[["pp_module1-patient_js"]])
    print(paste0("Selected SUBJECT: ", input[["pp_module1-patient_js"]]))
  })
  
  tendrilData <- reactive({
    
    req(myADAE(), input$armSelector1, input$armSelector2)
    
    myADAE() %>%
      filter( (TRTA %in% c(input$armSelector1, input$armSelector2) ) )
    
  })
  
  output$tendrilPlot <- renderPlotly({
    
    ae1 <- tendrilData()
    trts <- unique(ae1$TRTA)
    
    ae_tendril <- Tendril(
      mydata = as.data.frame(ae1),
      rotations = rep(3, nrow(ae1)),
      AEfreqTreshold = 9,
      Tag = "Comment",
      Treatments = trts,
      Unique.Subject.Identifier = "USUBJID",
      Terms = "AEDECOD",
      Treat = "TRTA",
      StartDay = "ASTDY",
      SubjList = as.data.frame(ae1%>%select(USUBJID, TRTA)),
      SubjList.subject = "USUBJID",
      SubjList.treatment = "TRTA"
    )
    
    plot1 <- plot.Tendril2(ae_tendril, interactive = TRUE, coloring = "p.adj") %>%
      layout(height = 600, width = 1000)
    
    onRender(plot1, "
      function(el, x){
        el.on('plotly_click', function(d){
          var selsub = d.points[0].customdata;
          console.log('d object= ', d);
          console.log('Subject id = ', selsub);
          Shiny.onInputChange('pp_module1-patient_js', selsub);
        })
      }
    ")
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

