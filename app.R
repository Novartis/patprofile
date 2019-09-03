if(file.exists('/CHBS/apps/busdev_apps/AVA/RLib/AVA.R')){
  source('/CHBS/apps/busdev_apps/AVA/RLib/AVA.R', local = T)
} else{
  source("http://ava-web.statwb.eu.novartis.net/cfg/installer.R")
}
library(shiny)
library(ava)

sub_mod <- function(input, output, session){
  return("SUCCESS SUB")
}
sub_UI <- function(id){
  ns <- NS(id)
  tabPanel(title = "| Submodule |",
           h4("Submod"))

}

test_mod <- function(input, output, session){
  ns <- session$ns
  callModule(module = sub_mod, id = "sub1")
  appendTab(inputId = "main_tab", sub_UI(ns("sub1")))
  return("SUCCESS")
}


test_UI <- function(id){
  ns <- NS(id)
  tagList(
    tabPanel(
      title = "Inside Test UI",
      wellPanel(h4("The well")),
      tabsetPanel(
        id = ns("main_tab"),
        tabPanel("Test1",
                 h4("Test 1")),
        tabPanel("Test2",
                 h4("Test 2"))
      )
    )
  )
}


################################################################################
# App
################################################################################
ui <- avaNavbarPage(
    id="Menu",
    tabPanel(
      title = "PatPro",
      test_UI(id = "test1")
  )
)

server <- function(input, output, session) {
  callModule(test_mod, "test1")
}

shinyApp(ui, server)


patientProfile_modUI <- function(id){
  print(session_info())
  ns <- NS(id)
  tagList(
    tabPanel(
      title = "Patient profiles",
      tabsetPanel(
        id = ns("main_tab"),
        tabPanel("Test1",
                 h4("Test 1")),
        tabPanel("Test2",
                 h4("Test 2"))
      )
    )
  )
}
patientProfile_mod <- function(input, output, session, uid = NULL,
                               ADSL = NULL, ADAE = NULL, ADCM = NULL,
                               ADLB = NULL, ADEG = NULL, ADPC = NULL, ADEX = NULL){
  return("RETURN")
}

ui2 <- avaNavbarPage(
  id="Menu",
  tabPanel(
    title = "Patient Profiles",
    patientProfile_modUI(id = "pp_module1")
  )
)

server2 <- function(input, output, session) {
  callModule(patientProfile_mod, "pp_module1")
}
shinyApp(ui2, server2)
