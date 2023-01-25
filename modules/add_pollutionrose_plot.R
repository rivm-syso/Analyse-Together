###############################################
### PollutionRose-plot Module ###
###############################################

# Creates a pollution rose plot -- openair

###################################################################
### Output Module ####
#################################################################

pollrose_output <- function(id) {

  ns <- NS(id)

  plotOutput(ns("pollrose_plot"))

}


######################################################################
# Server Module
######################################################################

pollrose_server <- function(id,
                            data_measurements,
                            data_stations,
                            data_other,
                            overview_component) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$pollrose_plot <- renderPlot({
      # Get the data to plot - stations
      data_plot <- data_measurements$data_filtered

      # Get the data to plot - KNMI wind data
      data_plot_wind <- data_measurements$data_filtered_knmi

      # Check if there is data to plot
      shiny::validate(
        need(!is_empty(data_plot) | !dim(data_plot)[1] == 0,
             'Geen sensordata beschikbaar.'),
        need(!is_empty(data_plot_wind) | !dim(data_plot_wind)[1] == 0,
             'Geen knmi-data beschikbaar.'),
      )

      # Get the colours for the stations
      data_stations <- data_stations$data %>%
        dplyr::select(c(station, col, linetype, size)) %>%
        dplyr::distinct(station, .keep_all = T) %>%
        dplyr::filter(!grepl("KNMI", station))

      # Determine parameter for the label in the plot
      parameter <- data_other$parameter

      # Find the corresponding label
      parameter_label <- overview_component %>%
        dplyr::filter(component == parameter) %>%
        dplyr::pull(label)

      # Get the stations data
      data_pollrose <- data_plot
      data_pollrose <- data_plot %>%
        dplyr::left_join(data_stations, by = "station")

      # Get the KNMI data
      knmidata <- data_plot_wind %>%
        dplyr::filter(parameter == 'wd' | parameter == 'ws') %>%
        dplyr::select(c('station', 'parameter', 'value', 'timestamp')) %>%
        tidyr::pivot_wider(names_from = 'parameter', values_from = 'value', values_fn = mean) %>%
        rename(knmi_stat = station)

      # Merge KNMI data with the stations data
      data_pollrose <- dplyr::left_join(data_pollrose, knmidata, by = 'timestamp', keep = T)

      # Make a plot ====
      try(pollutionRose(data_pollrose,
                        pollutant = "value",
                        wd = "wd",
                        ws = "ws",
                        type = 'station',
                        local.tz = "Europe/Amsterdam",
                        cols = "Oranges",
                        statistic = 'prop.mean',
                        breaks = c(0,10,25,50,100,200),
                        par.settings = list(fontsize=list(text=15)),
                        key = list(header = parameter_label,
                                   footer = '',
                                   labels = c('0 to 10', '10 to 25',
                                              '25 to 50','50 to 100', '100 or more')),
                        between = list(x=0.5, y = 0.5)))

    })

  })


}
