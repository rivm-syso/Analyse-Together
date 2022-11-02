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
#     verbatimTextOutput(ns('Click_text')),

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
                                 update_data,
                                 download_data_123,
                                 pool,
                                 measurements_con,
                                 stations_con,
                                 data_meta,
                                 selected_parameter,
                                 selected_time,
                                 default_time,
                                 select_mun_or_proj,
                                 choose_mun_or_proj,
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

                 # Load the data from the caching database ----
                 get_data <- reactive({
                  # Check if the download and update button is pushed
                   download_button <-  download_data_123()
                   shiny::validate(
                     need(( update_data() | download_button), "Klik op button update")
                   )

                   # Get the selected choice
                   type_choice <- choice_selection()

                   # Check if there is selected project/municipality
                   shiny::validate(
                     need(!is_empty((type_choice)),"Please, select gemeente of project")
                   )
                   if (type_choice == "Gemeente"){
                     type_choice <- "municipality"
                   }

                   # Get the selected time period
                   start_time <- get_time_selection()$start_time
                   end_time <- get_time_selection()$end_time

                   # Get the station names in the selected Municipality/project
                   stations_name <- get_stations_from_selection(mun_proj_select(), type_choice, conn = pool)

                   log_trace("mod comm: {lubridate::now()} Get data from caching database ... ")

                   # Get the data measurements of the selected Municipality/project
                   data_measurements <- measurements_con %>% as.data.frame() %>%
                     dplyr::mutate(date = lubridate::as_datetime(timestamp, tz = "Europe/Amsterdam"),
                                   # Add an uncertainty to the pm-measurements
                                   sd = dplyr::case_when(parameter == "pm25_kal" ~ 5.3,
                                                         parameter == "pm25" ~ 5.3,
                                                         parameter == "pm10_kal" ~ 8.5,
                                                         parameter == "pm10" ~ 8.5,
                                                         T ~ 0)) %>%
                     dplyr::filter(station %in% stations_name &
                                     date > start_time &
                                     date < end_time)

                   # Get the information from the sensors
                   data_sensors <- stations_con %>% as.data.frame() %>%
                     dplyr::filter(station %in% stations_name) %>%
                     dplyr::mutate(selected = F, col = col_default, linetype = line_default, station_type = "sensor") %>%
                     dplyr::mutate(station_type = ifelse(grepl("KNMI", station) == T, "KNMI", ifelse(grepl("NL", station) == T, "LML", station_type))) %>%
                     dplyr::mutate(linetype = ifelse(station_type == "LML", line_overload, linetype),
                            size = ifelse(station_type == "LML", 2,1))

                   log_trace("mod com: {lubridate::now()} Data available in tool. ")

                   return(list(data_measurements = data_measurements, data_sensors = data_sensors))
                 })

                # Get selected stations ----
                # TODO this needs to get some administration to select and deselect
                get_selected_station <- reactive({
                  selected_station <- get_stations_total() %>%
                    dplyr::filter(selected == T) %>%
                    dplyr::select(station) %>%
                    pull()
                  return(selected_station)
                })

                # Get total stations ----
                 # Get the total stations and their location, if they are selected, name/label and colour
                 # We assume that each station has only 1 location. Or we plot all, we don't distinguish location time
                 # TODO create a function or reactive to make this selection which locations to use
                 get_stations_total <- reactive({

                   update_data()
                   # Set selected stations to TRUE
                   # stations_total <- isolate(get_data())$data_sensors %>%
                   #   dplyr::mutate(selected = ifelse(station %in% c(selected_stations$state_station()),  T, selected))
                   data_snsrs <- try(isolate(get_data())$data_sensors, silent = T)
                   shiny::validate(
                     need(class(data_snsrs) != "try-error", "Error, no data selected.")
                   )

                   stations_total <- isolate(get_data())$data_sensors %>%
                      dplyr::mutate(selected = ifelse(station %in% c(selected_stations$state_station()),  T, selected))
                   # Assign colors -> sensor
                   stations_total <- assign_color_stations(stations_total, col_cat, col_default, col_overload, col_station_type = "sensor")

                   # Assign linetype -> reference station
                   stations_total <- assign_linetype_stations(stations_total, line_cat, line_default, line_overload, line_station_type = "ref")

                   log_trace("mod com: total number of stations {nrow(stations_total)}")

                   return(stations_total)
                 })

                 # Get time selection ----
                 # Get the start and end time from the user.
                 get_time_selection <- reactive({
                   start_time <- selected_time$selected_start_date()
                   end_time <- selected_time$selected_end_date()

                   # Check if a time is selected, otherwise total time
                   if(is.null(start_time)|is.null(end_time)){
                     start_time <- default_time$start_time
                     end_time <- default_time$end_time
                   }
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

                 # Fitler data measurements ----
                 # Reactive for the measurements to filter on input, time, map, component
                 filter_data_measurements <- reactive({

                   update_data()
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
                   all_data <- isolate(get_data()$data_measurements)
                   # Filter the measurements
                   measurements_filt_snsr <- all_data %>%
                     dplyr::filter(date > start_time & date < end_time &
                                     station %in% selected_stations &
                                     parameter == selected_parameter)

                  log_trace("mod com: number of selected stations {length(selected_stations)}")
                  log_trace("mod com: names of selected stations {paste(selected_stations, sep = ' ', collapse = ' ')}")
                  log_trace("mod com: filtered measurements {nrow(measurements_filt_snsr)}")
                   return(measurements_filt_snsr)
                 })

                 # Get knmi measurements ----
                 get_knmi_measurements <- reactive({
                   # Get the start and end time to filter on
                   update_data()
                   time_selected <- get_time_selection()
                   start_time <- time_selected$start_time
                   end_time <- time_selected$end_time

                   # Get selected stations
                   selected_stations <- get_selected_station()[grep("KNMI", get_selected_station())]

                   # TODO some check if time is available in data
                   # TODO check if selected sensors has data that time and component, otherwise a message?
                   # TODO for the selected stations and parameters connect with those selection modules
                   # Filter the measurements
                   measurements_filt <- isolate(get_data())$data_measurements %>%
                     dplyr::filter(date > start_time & date < end_time & station %in% selected_stations)

                   return(measurements_filt)
                 })

                # Obvserve download data ----
                  observeEvent(download_data_123(), {

                   get_data()

                 })


                # Return ----
                 return(list(
                  selected_time = reactive({get_time_selection()}),
                  station_locations = reactive({get_stations_total()}),
                  selected_measurements = reactive({filter_data_measurements()}),
                  choice_select = reactive({choice_selection()}),
                  mun_proj_select = reactive({mun_proj_select()}),
                  knmi_measurements = reactive({get_knmi_measurements()})
                  ))

               })
}
