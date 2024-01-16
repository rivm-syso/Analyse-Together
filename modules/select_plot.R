###############################################
### pickerInput - select plot ###
###############################################

# This is a plot selection module
######################################################################
# Output Module
######################################################################

plot_selection_output <- function(id) {

  ns <- NS(id)

  uiOutput(ns("plot_select"))

}


######################################################################
# Server Module
######################################################################

plot_selection_server <- function(id,
                                 data_other,
                                 plot_choices,
                                 default_plot) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns


    output$plot_select <- renderUI({
      # Create the plot picker with a list of possible choices
      tagList(

        pickerInput(
          ns("plot_select"),
          label    = i18n$t("sel_plot"),
          choices  = plot_choices,
          selected = default_plot,
          multiple = FALSE,
          options = pickerOptions(maxOptions = 1),
          width = "200px"
        )
      )
    })

    observeEvent(input$plot_select,{

      data_other$plot <- input$plot_select

    })
  })

}
