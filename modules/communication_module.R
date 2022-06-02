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
                                 # TODO Get the selected parameter form the module XXX
                                 selected_parameter ,
                                 # TODO Get the selected timeperiod from the module XXX
                                 selected_time ,
                                 # TODO Get the selected stations form the map
                                 selected_stations
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


                 # Get the total stations and their location, if they are selected, name/label and colour
                 # We assume that each station has only 1 location. Or we plot all, we don't distinguish location time
                 # TODO create a function or reactive to make this selection which locations to use
                 get_stations_total <- reactive({
                   browser()
                   # Set selected stations to TRUE
                   stations_total <- data_stations %>%
                     dplyr::mutate(selected = case_when(station %in% selected_stations ~ T,
                                                 T ~ F))
                   # Assign colors -> sensor
                   stations_total <- assign_color_stations(stations_total, col_cat, col_default, col_overload, col_station_type = "sensor")

                   # Assign linetype -> reference station
                   stations_total <- assign_linetype_stations(stations_total, line_cat, line_default, line_overload, line_station_type = "ref")

                   return(stations_total)
                 })

                 # Get the start and end time from the user.
                 # TODO if there is no selection by the user, does the time selection module gives the total time ?
                 # If not, then insert such a check here.
                 # Otherwise this reactive isnt needed
                 get_time_selection <- reactive({
                   start_time <- selected_time$selected_start_date()
                   end_time <- selected_time$selected_end_date()

                   # Check if a time is selected, otherwise
                   if(is.null(start_time)|is.null(end_time)){
                     start_time <- get_time_total()$start_time
                     end_time <- get_time_total()$end_time
                   }
                   return(list(start_time = start_time, end_time = end_time))
                 })

                 # Reactive for the measurements to filter on input, time, map, component
                 filter_data_measurements <- reactive({
                   # Get the start and end time to filter on
                   time_selected <- get_time_selection()
                   start_time <- time_selected$start_time
                   end_time <- time_selected$end_time
                    browser()
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

                return(list(
                  start_end_total = reactive({get_time_total()}),
                  selected_time = selected_time,
                  station_locations = reactive({get_stations_total()}),
                  selected_measurements = reactive({filter_data_measurements()})
                  ))

               })
}

