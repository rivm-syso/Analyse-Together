###############################################
### PollutionRose-plot Module ###
###############################################

# Creates a pollution rose plot -- openair

###################################################################
### Output Module ####
#################################################################

calender_output <- function(id) {

  ns <- NS(id)

  plotOutput(ns("calender_plot"))

}


######################################################################
# Server Module
######################################################################

calender_server <- function(id,
                            data_measurements,
                            data_measurements_knmi,
                            parameter,
                            overview_component) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$calender_plot <- renderPlot({
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
      data_calender <- data_plot

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
      data_calender <- dplyr::left_join(data_calender, knmidata, by = 'date')

      # Make a plot ====
      try(calendarPlot(data_calender, pollutant = "value", annotate = "ws",
                       type = 'label',
                       local.tz = "Europe/Amsterdam",
                       cols = "Oranges",
                       breaks = c(0,5,10,15,20,30,50,100, 300),
                       par.settings = list(fontsize=list(text=15)),
                       key.header = parameter_label,
                       key.footer = '',
                       main = paste0('Period: ', min(data_plot$date) %>% format("%d/%b/%Y"),
                                      " - ",  max(data_plot$date) %>% format("%d/%b/%Y")),
                       labels = c('0 to 5', '5 to 10', '10 to 15', '15 to 20', '20 to 30', '30 to 50', '50 to 100', '100 or more')))

    })

  })


}
