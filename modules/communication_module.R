###############################################
### Communication Module ###
###############################################

#The communication module covers all the communication between selection and
#visualisation. (The data, selection options and visualisation.) It stores for
#example which sensors are selected.

###################################################################
### Output Module ####
#################################################################
communication_output <- function(id) {

  ns <- NS(id)

  tagList(
    verbatimTextOutput(ns('Click_text')),

    tableOutput(ns("test_data_select_time")),
    tableOutput(ns("test_data_select_sensor")),
    tableOutput(ns("test_stations_total")),
    tableOutput(ns("test_startendtime"))
  )

}


######################################################################
# Server Module
######################################################################
#
#

communication_server <- function(id,
                                 data_measurements,
                                 data_stations,
                                 data_meta,
                                 selected_parameter ,
                                 selected_time ,
                                 # TODO Get the selected stations form the map
                                 selected_stations,
                                 # Options for the colors
                                 col_cat,
                                 col_default,
                                 col_overload,
                                 # Options for the linetype
                                 line_cat,
                                 line_default,
                                 line_overload
                                 ) {

  moduleServer(id,
               function(input, output, session) {

                 ns <- session$ns

                # Get the total time of the measurements, those are the limits for the time selection picker
                 get_time_total <- reactive({
                  start_time <- data_measurements %>% select(date) %>% pull() %>% min()
                  end_time <- data_measurements %>% select(date) %>% pull() %>% max()
                  return(list(start_time = start_time, end_time = end_time))
                })

                # TODO this needs to get some administration to selecet and deselect
                get_selected_station <- reactive({
                  selected_station <- get_stations_total() %>% dplyr::filter(selected) %>% dplyr::select(station) %>% pull()
                  return(selected_station)
                })

                 # Get the total stations and their location, if they are selected, name/label and colour
                 # We assume that each station has only 1 location. Or we plot all, we don't distinguish location time
                 # TODO create a function or reactive to make this selection which locations to use
                 get_stations_total <- reactive({
                   # Set selected stations to TRUE
                   stations_total <- data_stations %>%
                     dplyr::mutate(selected = case_when(station %in% c(selected_stations$state_station()) ~ T,
                                                 T ~ F))
                   # print(selected_stations$selected_station())
                   # Assign colors -> sensor
                   stations_total <- assign_color_stations(stations_total, col_cat, col_default, col_overload, col_station_type = "sensor")

                   # Assign linetype -> reference station
                   stations_total <- assign_linetype_stations(stations_total, line_cat, line_default, line_overload, line_station_type = "ref")

                   return(stations_total)
                 })

                 # Get the start and end time from the user.
                 get_time_selection <- reactive({
                   start_time <- selected_time$selected_start_date()
                   end_time <- selected_time$selected_end_date()

                   # Check if a time is selected, otherwise total time
                   if(is.null(start_time)|is.null(end_time)){
                     start_time <- get_time_total()$start_time
                     end_time <- get_time_total()$end_time
                   }
                   return(list(start_time = start_time, end_time = end_time))
                 })

                 # Get the parameter from the user
                 get_parameter_selection <- reactive({
                   parameter <- selected_parameter()
                   # Check if a parameter is selected, otherwise pm2.5-calibrated
                   if(is.null(parameter)){
                     parameter <- "pm25_kal"
                   }
                   return(list(parameter = parameter))
                 })


                 # Reactive for the measurements to filter on input, time, map, component
                 filter_data_measurements <- reactive({
                   # Get the start and end time to filter on
                   time_selected <- get_time_selection()
                   start_time <- time_selected$start_time
                   end_time <- time_selected$end_time
                   # Get the chosen parameter
                   selected_parameter <- get_parameter_selection()$parameter

                   # Get selected stations
                   selected_stations <- get_selected_station()

                   # TODO some check if time is available in data
                   # TODO check if selected sensors has data that time and component, otherwise a message?
                   # TODO for the selected stations and parameters connect with those selection modules
                   # Filter the measurements
                   measurements_filt <- data_measurements %>%
                     dplyr::filter(date > start_time & date < end_time & station %in% selected_stations & parameter == selected_parameter)
                   return(measurements_filt)
                 })

                 output$test_data_select_time <- renderTable({

                   test123 <- filter_data_measurements()
                   head(test123)
                   })
                 output$test_stations_total <- renderTable({
                   test123 <- get_stations_total()
                   head(test123)
                 })
                 output$test_data_select_sensor <- renderTable({
                   test123 <- get_stations_total() %>% filter(selected) %>%  dplyr::distinct()
                   test123}
                 )
                 output$test_startendtime <- renderTable({
                   test123 <- get_time_total()
                   test123}
                 )
                 output$Click_text <- renderText({
                   get_selected_station()
                 })

                return(list(
                  start_end_total = reactive({get_time_total()}),
                  selected_time = selected_time,
                  station_locations = reactive({get_stations_total()}),
                  selected_measurements = reactive({filter_data_measurements()})
                  ))

               })
}
