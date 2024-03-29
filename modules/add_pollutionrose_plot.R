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
                            data_measurements_knmi,
                            parameter,
                            overview_component) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$pollrose_plot <- renderPlot({
      # Get the data to plot - stations
      data_plot <- data_measurements()

      # Get the data to plot - KNMI wind data
      data_plot_wind <- data_measurements_knmi()

      # Check if there is data to plot
      shiny::validate(
        need(!is_empty(data_plot) | !dim(data_plot)[1] == 0,
             'Geen sensordata beschikbaar.'),
        need(!is_empty(data_plot_wind) | !dim(data_plot_wind)[1] == 0,
             'Geen knmi-data beschikbaar.'),
      )

      # Find the corresponding label, for the parameter
      parameter_label <- overview_component %>%
        dplyr::filter(component == parameter()) %>%
        dplyr::pull(label)

      # Get the stations data
      data_pollrose <- data_plot

      # Get the KNMI data
      knmidata <- data_plot_wind %>%
        dplyr::filter(parameter == 'wd' | parameter == 'ws') %>%
        dplyr::select(c('station', 'parameter', 'value', 'date')) %>%
        tidyr::pivot_wider(names_from = 'parameter', values_from = 'value', values_fn = mean) %>%
        rename(knmi_stat = station)

      # Check if there is only 1 knmi station is selected
      knmi_number <- knmidata %>% dplyr::select(knmi_stat) %>% unique() %>% count() %>% pull()
      shiny::validate(need(knmi_number == 1, "Please select only 1 KNMI station."))

      # Merge KNMI data with the stations data
      data_pollrose <- dplyr::left_join(data_pollrose, knmidata, by = 'date')

      # Make a plot ====
      try(openair::pollutionRose(data_pollrose,
                        pollutant = "value",
                        wd = "wd",
                        ws = "ws",
                        type = 'label',
                        local.tz = "Europe/Amsterdam",
                        cols = "Oranges",
                        statistic = 'prop.mean',
                        breaks = c(0,12,30,60,1000),
                        par.settings = list(fontsize=list(text=15)),
                        main = paste0('Period: ', min(data_plot$date) %>% format("%d/%b/%Y"),
                                      " - ",  max(data_plot$date) %>% format("%d/%b/%Y")),

                        key = list(header = parameter_label,
                                   footer = '',
                                   labels = c('0 to 12', '12 to 30',
                                              '30 to 60','60 or more')),
                        between = list(x=0.5, y = 0.5)))

    })

  })


}
