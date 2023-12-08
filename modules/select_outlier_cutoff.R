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
      tagList(

        numericInput(
          ns("outlier_cutoff"),
          label  = i18n$t("sel_cutoff"),
          value  = default_cutoff,
          min = 0.1
        )
      )
    })

    observeEvent(input$outlier_cutoff,{

      data_other$cutoff <- input$outlier_cutoff

    })
  })

}

