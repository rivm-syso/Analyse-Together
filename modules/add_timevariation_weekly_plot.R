###############################################
### TimeVariation-plot Module: weekly ###
###############################################

# Creates a timevariation-plot with the chosen stations, parameters and time range
# Weekly version!
###################################################################
### Output Module ####
#################################################################

timevar_weekly_output <- function(id) {

  ns <- NS(id)
  plotOutput(ns("timevar_plot_weekly"))

}


######################################################################
# Server Module
######################################################################

timevar_weekly_server <- function(id,
                                  data_measurements,
                                  parameter,
                                  overview_component) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$timevar_plot_weekly <- renderPlot({
      # Get the data to plot
      data_plot <- data_measurements()

      # Check if there is data to plot
      shiny::validate(
        need(!is_empty(data_plot) | !dim(data_plot)[1] == 0,
             'Geen sensordata beschikbaar.')
      )

      # Determine parameter for the label in the plot
      parameter <- parameter()

      # Find the corresponding label
      parameter_label <- overview_component %>%
        dplyr::filter(component == parameter) %>%
        dplyr::pull(label)


      # Make a plot
      plot_all <- timeVariation(data_plot,
                    pollutant = "value",
                    normalise = FALSE,
                    group = "label",
                    alpha = 0.1,
                    cols = data_plot$col,
                    local.tz = "Europe/Amsterdam",
                    ylim = c(0,NA),
                    ylab = parameter_label,
                    start.day = 1,
                    par.settings = list(fontsize=list(text=15)),
                    key = T
                    )

      # Get a part of the plot - weekly
      plot_all[[1]][1]

    })

  })

}
