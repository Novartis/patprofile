function(request){
  avaNavbarPage(
    id="Menu",
    tabPanel(
      title = "Patient Profiles",
      patientProfile_modUI(id = "pp_module1")
    ),
    tabPanel(
      title = "Linked plotly",
      tagList(
        p("Click a point on the scatterplot of lab values to select a subject in the patient profile"),
        plotlyOutput(outputId = "linked_ply"),
        verbatimTextOutput("click")
      )
    ),
    tabPanel(
      title = "Wiki",
      tagList(
        p("Development app for the modularization of the patient profiles"),
        p("This app for demonstration purpose only. It is pre-loaded with all relevant datasets."))
    )
  )
}
