###############################################
### Info sensor Module ###
###############################################

# Creates a calender plot -- openair
# To show the user which data is loaded in the tool, the number of stations
# measurering PM

###################################################################
### Output Module ####
#################################################################

info_sensor_output <- function(id) {

  ns <- NS(id)

  plotOutput(ns("info_sensor_plot"))

}


######################################################################
# Server Module
######################################################################

info_sensor_server <- function(id,
                            data_measurements) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$info_sensor_plot <- renderPlot({
      # Get the data to plot - stations
      data_plot <- data_measurements()

      # Check if there is data to plot
      shiny::validate(
        need(!is_empty(data_plot) | !dim(data_plot)[1] == 0,
             'Klik op start')
      )

      # Get the number of stations measuring pm
      data_calender <- data_plot %>%
        dplyr::filter(grepl("pm", parameter)) %>%
        dplyr::select(c(station, date)) %>%
        unique() %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(count_stations = n()) %>%
        dplyr::ungroup()

      # Make a plot ====
      try(calendarPlot(data_calender, pollutant = "count_stations",
                       type = 'label',
                       local.tz = "Europe/Amsterdam",
                       cols = "Oranges",
                       breaks = c(0,10,30,100, 800),
                       par.settings = list(fontsize=list(text=15)),
                       key.header = "Number of stations",
                       key.footer = '',
                       key.position = 'bottom',
                       labels = c('0-10', '10-30', '30-100',  '100+')))

    })

  })


}
