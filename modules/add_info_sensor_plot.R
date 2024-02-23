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
        need(nrow(data_plot) > 0,
             'Geen data beschikbaar')
      )

      # Get the number of stations measuring pm and not NL-stations (ref stations)
      data_calender <- data_plot %>%
        dplyr::filter(grepl("pm", parameter) & !grepl("NL", station)) %>%
        dplyr::select(c(station, date)) %>%
        unique() %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(count_stations = n()) %>%
        dplyr::ungroup()

      # Set the breaks
      max_n_stations <- max(data_calender$count_stations)
      breaks_values <- seq(from = 0, to = max_n_stations, length.out = 5) %>% round(0)
      breaks_labels <- paste0(breaks_values[1:4], "-", breaks_values[2:5])

      # If there are too few sensors for 5 categories, create less breaks
      if(T %in% duplicated(breaks_values)){
        breaks_values <- seq(from = 0, to = max_n_stations, by = 1)
        nr_labels <- length(breaks_values)
        breaks_labels <- paste0(breaks_values[1:nr_labels-1], "-", breaks_values[2:nr_labels])
      }


      # Make a plot ====
      try(calendarPlot(data_calender, pollutant = "count_stations",
                       type = 'label',
                       local.tz = "Europe/Amsterdam",
                       cols = c("#fff4dc","#ffe9b7","#ffb612","#c58800"),
                       breaks = breaks_values,
                       par.settings = list(fontsize=list(text=15)),
                       key.header = "Number of stations",
                       key.footer = '',
                       key.position = 'bottom',
                       labels = breaks_labels
                       )
                       )

    })

  })


}
