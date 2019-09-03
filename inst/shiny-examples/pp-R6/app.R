library(ava, lib.loc = "/home/engst2/Rlibs/3.4")
library(shiny)
library(pprofile)





mod <- patProfileMod2$new(id = "pp_module1")

ui <- function(request){
  avaNavbarPage(
  id="Menu",
  tabPanel(
    title = "Patient Profiles R6",
    mod$ui()
  ),
  tabPanel(
    title = "About",
    tagList(p("Example app for the R6 implementation of the patient profiles module"))
  )
)
}

server <- function(input, output, session){
  myADSL <- reactive({
    adslf <- system.file("data/ADSL.sas7bdat", package = "pprofile")
    haven::read_sas(adslf)
  })
  myADLB <- reactive({
    adlbf <- system.file("data/ADLB.sas7bdat", package = "pprofile")
    haven::read_sas(adlbf)
  })

  mod$callModule()
  observe({
    mod$updateInputPort(id = "ADSL", input = myADSL)
    myADSL %>1% mod
    myADLB %>2% mod
    mod$updateInputPort(id = "ADLB", input = myADLB)
  })
}

shinyApp(ui, server)
