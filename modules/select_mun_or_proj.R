###############################################
### pickerInput - select component ###
###############################################

# This is a project selection module
######################################################################
# Output Module
######################################################################

project_or_mun_selection_output <- function(id) {

  ns <- NS(id)

  uiOutput(ns("proj_or_mun_select"))

}


######################################################################
# Server Module
######################################################################

project_or_mun_selection_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$proj_or_mun_select <- renderUI({

      # Create the component picker with a list of possible choices
      tagList(

        pickerInput(
          ns("proj_or_mun_select"),
          label    = i18n$t("sel_basedon"),
          choices  = c(i18n$t("word_muni"), "Project"),
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(maxOptions = 1)
        )
      )
    })

    # Return the chosen component
    return(selected_proj_or_mun = reactive({input$proj_or_mun_select}))

  })

}

