function(request){
  navbarPage(
    
    title = "Patprofile with SafetyexploreR",
    theme = shinytheme("cerulean"),
    
    tabPanel(
      title = "Wiki",
      includeMarkdown("./wiki.md")
    ),
    
    tabPanel(
      title = "Patient Profiles",
      patientProfile_modUI(id = "pp_module1")
    ),
    
    navbarMenu("Adverse Events",
               tabPanel(
                 title = "AE Explorer",
                 aeExplorerOutput("aeexp")
               ),
               
               tabPanel(
                 title = "AE Timelnes",
                 aeTimelinesOutput("aetim")
               )
    ),
    
    navbarMenu("Lab Analysis",
               tabPanel(
                 title = "Outlier Explorer",
                 safetyOutlierExplorerOutput("outexp")
               ),
               
               tabPanel(
                 title = "Hepatoxicity eDISH",
                 eDISHOutput("edish")
               ),
               
               tabPanel(
                 title = "Shift Plot",
                 safetyShiftPlotOutput("shiftplot")
               ),
               
               tabPanel(
                 title = "Histogram",
                 safetyHistogramOutput("histplot")
               ),
               
               tabPanel(
                 title = "Boxplot",
                 safetyResultsOverTimeOutput("boxplot")
               )
    ),
    
    tabPanel(
      title = "Tendril plot",
      tagList(
        uiOutput("tendrilTab")
      )
    ),
    tabPanel(
      title = "eDISH using Plotly",
      tagList(
        p("Click a point on the scatterplot of lab values to select a subject in the patient profile"),
        plotlyOutput(outputId = "linked_ply", height=600, width=600),
        verbatimTextOutput("click")
      )
    )
  )
}
