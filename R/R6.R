# REPLACE CLASSIC WRAPPER
#' @importFrom R6 R6Class
#' @importFrom haven read_sas
#' @export
patProfileMod2 <- R6Class(
  "patProfileModule2",
  inherit = ava::Module,
  public = list(
    initialize = function(id = NULL){
      super$initialize(id)
      # INPUTS
      self$definePort({
        self$addInputPort(
          name = "ADSL",
          description = "reactive ADaM formatted subject level data",
          sample = read_sas(system.file("data/ADSL.sas7bdat", package = "pprofile")),
          input = NULL)
        self$addInputPort(
          name = "ADLB",
          description = "reactive ADaM formatted lab data",
          sample =read_sas(system.file("data/ADLB.sas7bdat", package = "pprofile")),
          input = NULL)
        # No output port
      })
    }, #End initialize
    ui = function(id = id) {
      tagList(
        tabPanel(
          title = "Patient profiles",
          wellPanel(
            sidebarLayout(
              sidebarPanel(
                width = 3, align = "center",
                uiOutput(self$ns("uio_patient_selector"))
              ),
              mainPanel(
                uiOutput(self$ns("uio_patient_summ"))
              )
            )
          ),
          tabsetPanel(
            id = self$ns("main_tabset"),
            tabPanel(title = "Graphical overview")
          )
        )
      )
    },
    server = function(input, output, session,
                      ADSL = NULL, ADLB = NULL,
                      options = NULL){
      # INPUTS
      self$assignPort({
        self$updateInputPort(
          id = "ADSL",
          input = ADSL)
        self$updateInputPort(
          id = "ADLB",
          input = ADLB)

        self$updateOptionPort(
          option = options
        )
      })
      # SERVER CODE
      profile_dat <- reactiveValues() # reactiveValues are visible within the module server

      observe({
        profile_dat$ADSL <- self$getInput("ADSL")()
      })


      observe({
        print("Found ADSL")
        sl <- self$getInput("ADSL")()

        profile_dat$ADSL <- as.data.frame(profile_dat$ADSL)
        profile_dat$patient_list <- unique(profile_dat$ADSL$USUBJID)
      })

      # List patients
      output$uio_patient_selector <- renderUI({
        tagList(
          selectInput(inputId = self$ns("patient_selector"), label = "Select patient", choices = profile_dat$patient_list)
        )
      })

      output$uio_patient_summ <- renderUI({
        print(paste("renderUI: Patient summary", input$patient_selector))

        selected_ADSL <- profile_dat$ADSL
        selected_ADSL <- selected_ADSL[selected_ADSL$USUBJID == input$patient_selector,]
        selected_ADSL <- selected_ADSL[1,] #For studies with multiple entries
        patient_summary_txt <- .get_summary_text(selected_ADSL)
        output$patient_summary <- renderText(patient_summary_txt)
        htmlOutput(self$ns("patient_summary"))
      })

      uid <- reactiveVal()
      observe({uid(input$patient_selector)}) #Always write current patient to uid
      observe({
        myADLB <- self$getInput("ADLB")
        print(paste("Call submod LABO. uid:", uid()))
        print(self$ns("main_tabset"))
        callModule(module = labo_mod, id = "labo_auto", uid = uid, ADLB = myADLB)
        appendTab(inputId = "main_tabset", tabPanel(title = "Lab Overview", labo_UI(self$ns("labo_auto"))))
      })

      return("SERVER SUCCESS")
    } # End server
  ),
  private = list(
    # Private methods
  )
)

