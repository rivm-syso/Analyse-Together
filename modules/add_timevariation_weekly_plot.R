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
#
#

timevar_weekly_server <- function(id, com_module) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Determine parameter that needs to be plotted
    # Get selected measurements from communication module
    data_measurements <- reactive({
      data_measurements <- com_module$selected_measurements()
      return(data_measurements)
    })

    # Get selected stations from communication module
    data_stations <- reactive({
      data_stations <- com_module$station_locations() %>% select(c(station, col)) %>% dplyr::distinct(station, .keep_all = T)
      return(data_stations)
    })

    output$timevar_plot_weekly <- renderPlot({

      # Check if there is data to plot
      shiny::validate(
        need(!is_empty(data_measurements()),'Geen sensordata beschikbaar.'),
        need(!dim(data_measurements())[1] == 0,'Geen sensordata beschikbaar.')
      )

      # Determine parameter for the label in the plot
      parameter <- com_module$selected_parameter()$parameter

      # Find the corresponding label
      parameter_label <- overview_component %>%
        dplyr::filter(component == parameter) %>%
        dplyr::pull(label)

      # Get measurements stations
      data_timevar <- data_measurements()
      data_timevar <- merge(data_timevar, data_stations(), by = 'station')

      # Make a plot
      data_timevar$value <- as.numeric(data_timevar$value)

      plot_all <- timeVariation(data_timevar,
                    pollutant = "value", normalise = FALSE, group = "station",
                    alpha = 0.1, cols = data_timevar$col, local.tz="Europe/Amsterdam",
                    ylim = c(0,NA),
                    ylab = parameter_label,
                    start.day = 1,
                    par.settings=list(fontsize=list(text=15)),
                    key = T
                    )

      # Get a part of the plot
      plot_all[[1]][1]

    })

  })

}
