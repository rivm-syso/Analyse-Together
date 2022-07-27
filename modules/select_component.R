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

component_selection_server <- function(id, comp_choices) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$comp_select <- renderUI({

      # Create the component picker with a list of possible choices
      tagList(

        pickerInput(
          ns("comp_select"),
          label    = "Select component",
          choices  = comp_choices,
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(maxOptions = 1)
          )
        )
      })

    # Return the chosen component
    return(selected_component = reactive({input$comp_select}))

    })

  }

