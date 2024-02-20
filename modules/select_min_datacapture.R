###############################################
### numericInput - minimum data capture ###
###############################################

# This module the user can put a value which indicates the minimum value of the
# data capture, stations with less data capture are excluded from the analysis,
# that means they are deselected.

######################################################################
# Output Module
######################################################################

min_datacapture_output <- function(id) {

  ns <- NS(id)

  uiOutput(ns("min_datacapture"))

}


######################################################################
# Server Module
######################################################################

min_datacapture_server <- function(id,
                                  data_other,
                                  default_capture) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns


    output$min_datacapture <- renderUI({
      # Create the component picker with a list of possible choices
      tagList(

        numericInput(
          ns("min_datacapture"),
          label  = "Choose minimum value data capture: ",#i18n$t("sel_cutoff"),
          value  = default_capture,
          width = "200px",
          min = 0
        )
      )
    })

    observeEvent(input$min_datacapture,{

      data_other$min_capture <- input$min_datacapture

    })
  })

}
