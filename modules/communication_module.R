###############################################
### Communication Module ####
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
    #     verbatimTextOutput(ns('Click_text')),

    tableOutput(ns("test_data_select_time")),
    tableOutput(ns("test_data_select_sensor")),
    tableOutput(ns("test_stations_total")),
    tableOutput(ns("test_startendtime"))
  )

}


######################################################################
### Server Module #####
######################################################################
#
#

communication_server <- function(id,
                                 data_measurements,
                                 data_stations,
                                 data_meta,
                                 selected_parameter,
                                 selected_time,
                                 default_time,
                                 select_mun_or_proj,
                                 choose_mun_or_proj
) {

  moduleServer(id,
               function(input, output, session) {

                 ns <- session$ns

                 # Get selected stations ----
                 get_selected_station <- reactive({
                   shiny::validate(need(!is.null(data_stations$data), "No data_stations"))
                   selected_station <- data_stations$data %>%
                     dplyr::filter(selected == T) %>%
                     dplyr::select(station) %>%
                     pull()

                   log_trace("mod com: selected stations {selected_station}")

                   return(selected_station)
                 })

                 # Get time selection ----
                 # Get the start and end time from the user.
                 get_time_selection <- reactive({
                   start_time <- selected_time$selected_start_date()
                   end_time <- selected_time$selected_end_date()

                   log_trace("mod com: selected time range {start_time} - {end_time}")

                   return(list(start_time = start_time, end_time = end_time))
                 })

                 # Get parameter selection ----
                 # Get the parameter from the user
                 get_parameter_selection <- reactive({
                   parameter <- selected_parameter()
                   # Check if a parameter is selected, otherwise pm2.5-calibrated
                   if(is.null(parameter)){
                     parameter <- "pm25_kal"
                   }

                   log_trace("mod com: selected parameter {parameter}")

                   return(list(parameter = parameter))
                 })

                 # Get choice selection ----
                 # Get the type mun/proj selection from the user
                 choice_selection <- reactive({
                   selected <- select_mun_or_proj()
                   return(selected)
                 })

                 # Get name project/municipality ----
                 # Get the name of the mun/proj selection from the user
                 mun_proj_select <- reactive({
                   selected <- choose_mun_or_proj()
                   return(selected)
                 })

                 # Filter data measurements ----
                 # Reactive for the measurements to filter on input, time, stations, component
                 filter_data_measurements <- reactive({
                   browser()
                   # Get the start and end time to filter on
                   start_time <- selected_time$selected_start_date()
                   end_time <- selected_time$selected_end_date()

                   # Get the chosen parameter
                   selected_parameter <- get_parameter_selection()$parameter

                   # Get selected stations
                   selected_stations <- get_selected_station()

                   # Get the data
                   data_all <- data_measurements$data_all

                   # Check if everything is available for the selection
                   shiny::validate(need(!is.null(start_time) &
                                          !is.null(end_time) &
                                          !is.null(selected_parameter) &
                                          !is.null(data_all) &
                                          !purrr::is_empty(selected_stations),
                                        "Not yet data selected" ) )

                   # Filter the measurements
                   measurements_filt_stns <- data_all %>%
                     dplyr::filter(date > start_time & date < end_time &
                                     station %in% selected_stations &
                                     parameter == selected_parameter)

                   # log_trace("mod com: number of selected stations {length(selected_stations)}")
                   # log_trace("mod com: names of selected stations {paste(selected_stations, sep = ' ', collapse = ' ')}")
                   log_trace("mod com: filtered measurements {nrow(measurements_filt_stns)}")
                   return(measurements_filt_stns)
                 })

                 # Get knmi measurements ----
                 # the knmi measurements are excluded
                 # by the selected parameter in the measurements_filt_stns
                 get_knmi_measurements <- reactive({
                   # Get the start and end time to filter on
                   start_time <- selected_time$selected_start_date()
                   end_time <- selected_time$selected_end_date()

                   # Get selected stations
                   all_selected_stations <- get_selected_station()
                   selected_stations <- all_selected_stations[grep("KNMI", all_selected_stations)]

                   # Get the data
                   data_all <- data_measurements$data_all
                   browser()

                   # Check if everything is available for the selection
                   shiny::validate(need(!is.null(start_time) &
                                          !is.null(end_time) &
                                          !is.null(data_all) &
                                          !purrr::is_empty(selected_stations),
                                        "Not yet data selected" ) )

                   # Filter the measurements
                   measurements_filt_knmi <- data_all %>%
                     dplyr::filter(date > start_time & date < end_time &
                                   station %in% selected_stations
                     )

                   # log_trace("mod com: number of selected stations {length(selected_stations)}")
                   # log_trace("mod com: names of selected stations {paste(selected_stations, sep = ' ', collapse = ' ')}")
                   log_trace("mod com: filtered measurements KNMI {nrow(measurements_filt_knmi)}")

                   return(measurements_filt_knmi)
                 })


                 # Return ----
                 return(list(
                   selected_time = reactive({get_time_selection()}),
                   choice_select = reactive({choice_selection()}),
                   selected_measurements = reactive({filter_data_measurements()}),
                   knmi_measurements = reactive({get_knmi_measurements()}),
                   selected_parameter = reactive({get_parameter_selection()})
                 ))

               })
}
