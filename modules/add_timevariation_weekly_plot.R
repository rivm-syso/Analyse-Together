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
                                  data_stations,
                                  data_other,
                                  overview_component) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$timevar_plot_weekly <- renderPlot({
      # Get the data to plot
      data_plot <- data_measurements$data_filtered

      # Get the colours for the stations
      data_stations <- data_stations$data %>%
        dplyr::select(c(station, col, linetype, size)) %>%
        dplyr::distinct(station, .keep_all = T) %>%
        dplyr::filter(!grepl("KNMI", station))

      # Check if there is data to plot
      shiny::validate(
        need(!is_empty(data_plot),'Geen sensordata beschikbaar.'),
        need(!dim(data_plot)[1] == 0,'Geen sensordata beschikbaar.')
      )

      # Determine parameter for the label in the plot
      parameter <- data_other$parameter

      # Find the corresponding label
      parameter_label <- overview_component %>%
        dplyr::filter(component == parameter) %>%
        dplyr::pull(label)

      # Get measurements stations
      data_timevar <- data_plot
      data_timevar <- data_plot %>%
        dplyr::left_join(data_stations, by = "station")

      # Make a plot
      data_timevar$value <- as.numeric(data_timevar$value)

      plot_all <- timeVariation(data_timevar,
                    pollutant = "value",
                    normalise = FALSE,
                    group = "station",
                    alpha = 0.1,
                    cols = data_timevar$col,
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
