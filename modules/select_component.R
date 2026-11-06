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
      comp_label <- i18n$t("sel_comp")

      tagList(

        pickerInput(
          ns("comp_select"),
          label    = comp_label,
          choices  = comp_choices,
          selected = default_parameter,
          multiple = TRUE,
          width = "500px",
          options = pickerOptions(maxOptions = 1)
          ),

        # set aria-label instead
        tags$script(HTML(sprintf(
          "setTimeout(function() {
             $('select#%s').siblings('button').attr('aria-label', %s);
           }, 0);",
          ns("comp_select"),
          jsonlite::toJSON(comp_label, auto_unbox = TRUE)
        )))
        )
      })

    observeEvent(input$comp_select,{

      data_other$parameter <- input$comp_select

    })
    })

  }

