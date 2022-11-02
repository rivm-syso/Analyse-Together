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
#
#

pollrose_server <- function(id, com_module) {

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
      data_stations <- com_module$station_locations() %>%
        dplyr::select(c(station, col)) %>%
        dplyr::distinct(station, .keep_all = T)
      return(data_stations)
    })

    # Get the KNMI data from the communication module
    data_knmi <- reactive({
      data_knmi <- com_module$knmi_measurements()
      return(data_knmi)
    })

    output$pollrose_plot <- renderPlot({
      # Check if there is data to plot
      shiny::validate(
        need(!is_empty(data_measurements()),'Geen sensordata beschikbaar.'),
        need(!dim(data_measurements())[1] == 0,'Geen sensordata beschikbaar.'),
        need(!is_empty(data_knmi()),'Geen knmi-data beschikbaar.'),
        need(!dim(data_knmi())[1] == 0,'Geen knmi-data beschikbaar.')
      )

      # Determine parameter for the label in the plot
      parameter <- com_module$selected_parameter()$parameter

      # Find the corresponding label
      parameter_label <- overview_component %>%
        dplyr::filter(component == parameter) %>%
        dplyr::pull(label)

      # Get the stations data
      data_pollrose <- data_measurements()
      data_pollrose <- merge(data_pollrose, data_stations(), by = 'station')

      # Get the KNMI data
      knmidata <- data_knmi() %>%
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
                        local.tz="Europe/Amsterdam",
                        cols = "Oranges",
                        statistic = 'prop.mean',
                        breaks=c(0,10,25,50,100,200),
                        par.settings=list(fontsize=list(text=15)),
                        key = list(header = parameter_label,
                                   footer = '',
                                   labels = c('0 to 10', '10 to 25',
                                              '25 to 50','50 to 100', '100 or more')),
                        between = list(x=0.5, y = 0.5)))

    })

  })


}
