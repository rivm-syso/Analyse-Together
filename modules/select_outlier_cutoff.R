###############################################
### numericInput - outlier value cut-off ###
###############################################

# This module the user can put a value which indicates the maximum reliable value.
# Values greater than this value are seen as outliers and excludeed in the analyses.
######################################################################
# Output Module
######################################################################

outlier_cutoff_output <- function(id) {

  ns <- NS(id)

  uiOutput(ns("outlier_cutoff"))

}


######################################################################
# Server Module
######################################################################

outlier_cutoff_server <- function(id,
                                 data_other,
                                 default_cutoff) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns


    output$outlier_cutoff <- renderUI({
      # Create the component picker with a list of possible choices
      # input id differs from the uiOutput id to avoid duplicate DOM ids
      cutoff_input <- numericInput(
        ns("outlier_cutoff_input"),
        label  = i18n$t("sel_cutoff"),
        value  = default_cutoff,
        width = "500px",
        min = 0.1
      )

      # add autocomplete attribute to the actual <input> for accessibility
      cutoff_input <- htmltools::tagQuery(cutoff_input)$
        find("input")$
        addAttrs(autocomplete = "off")$
        allTags()

      tagList(cutoff_input)
    })

    observeEvent(input$outlier_cutoff_input,{

      data_other$cutoff <- input$outlier_cutoff_input

    })
  })

}

