###############################################
### Overview Time Series Plot Module ###
###############################################

# Creates a time series plot with all the stations and the mean of the sensors

###################################################################
### Output Module ####
#################################################################

overview_timeseries_output <- function(id) {

  ns <- NS(id)

  timeseries_output(ns("overview_timeseries_plot"))

}


######################################################################
# Server Module
######################################################################

overview_timeseries_server <- function(id,
                                       data_stations,
                                      data_measurements_all,
                                      parameter,
                                      selected_cutoff,
                                      overview_component,
                                      theme_plots,
                                      change_tab,
                                      zoom_in = NA
                                      ){

  moduleServer(id, function(input, output, session) {

    ns <- session$ns


    # Calc mean and change colours
    data_plot <- reactive({
      # Get all the data
      measurements <- data_measurements_all()

      # Get all the stations and there type
      station_info <- data_stations()

      # Check if there is data
      shiny::validate(need(nrow(measurements) > 0, "No data available to plot."))
      shiny::validate(need(nrow(station_info) > 0, "No data available to plot."))

      # Add station type to the measurements
      measurements <-
        dplyr::left_join(measurements,
                         station_info, by = "station") %>%
        dplyr::select(station, date, parameter, value, sd, label,
                      group_name, col, size, station_type, linetype) %>%
        # Keep for this dataset the label the same as the station. No changes for grouping yet.
        dplyr::mutate(label = station)

      # Change colour to light grey for the sensors
      measurements <- measurements %>%
        dplyr::mutate(col = ifelse(station_type == "sensor","#9a9a9a", col)) %>%
        # Show only the sensors
        dplyr::filter(station_type == "sensor") %>%
        # Select only the selected parameter
        dplyr::filter(parameter == parameter()) %>%
        # Remove values above cutoff value
        dplyr::filter(value < selected_cutoff())

      # Calc mean of the sensors
      mean_measurements <- measurements %>%
        # Keep also the parameters for the plotting
        dplyr::group_by(date, parameter, station_type, linetype) %>%
        dplyr::summarise(value = mean(value, na.rm = T),
                         number = n(),
                         sd = mean(sd, na.rm = T)/sqrt(n())) %>%
        dplyr::ungroup() %>%
        # Add some columns for the plotting
        dplyr::mutate(col = "black",
                      size = 1,
                      label = "mean",
                      group_name = "mean",
                      station = "mean")

      # Add the mean to the data
      measurements <- measurements %>% dplyr::bind_rows(mean_measurements)
    })

    observeEvent(change_tab(),{
      # Make plot
      timeseries_server("overview_timeseries_plot",
                        data_measurements = reactive(data_plot()),
                        parameter = parameter,
                        overview_component = overview_component,
                        theme_plots = theme_plots,
                        remove_legend = TRUE,
                        manual_ylim = c(0,selected_cutoff() + 5))

    })



    })



}
