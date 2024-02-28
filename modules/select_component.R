###############################################
### pickerInput - select component ###
###############################################

# This is a component selection module
######################################################################
# Output Module
######################################################################

component_selection_output <- function(id) {

  ns <- NS(id)

  uiOutput(ns("comp_select"))

}


######################################################################
# Server Module
######################################################################

component_selection_server <- function(id,
                                       data_other,
                                       comp_choices,
                                       default_parameter) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns


    output$comp_select <- renderUI({
      # Create the component picker with a list of possible choices
      tagList(

        pickerInput(
          ns("comp_select"),
          label    = i18n$t("sel_comp"),
          choices  = comp_choices,
          selected = default_parameter,
          multiple = TRUE,
          options = pickerOptions(maxOptions = 1)
          )
        )
      })

    observeEvent(input$comp_select,{

      data_other$parameter <- input$comp_select

    })
    })

  }

