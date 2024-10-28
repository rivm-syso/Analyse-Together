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

}


######################################################################
### Server Module #####
######################################################################
#
#

communication_server <- function(id,
                                 data_measurements, # class: "reactiveExpr" "reactive" "function"
                                 data_stations, # class: "reactiveExpr" "reactive" "function"
                                 data_meta,
                                 selected_parameter,
                                 selected_start_date,
                                 selected_end_date,
                                 selected_cutoff
                                 )
  {

  moduleServer(id,
               function(input, output, session) {

                 ns <- session$ns

                 # Get selected stations ----
                 get_selected_station <- reactive({
                   shiny::validate(need(!is.null(data_stations()), "No data_stations"))
                   selected_station <- data_stations() %>%
                     dplyr::filter(selected == T) %>%
                     dplyr::select(station) %>%
                     pull()

                   log_trace("mod com: selected stations {selected_station}")

                   return(selected_station)

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

                 # Filter data measurements ----
                 # Reactive for the measurements to filter on input, time,
                 # stations, component
                 filter_data_measurements <- reactive({
                   # Get the start and end time to filter on
                   start_time <- selected_start_date()
                   end_time <- selected_end_date()

                   # Get the chosen parameter
                   selected_parameter <- get_parameter_selection()$parameter

                   # Get selected stations
                   selected_stations <- get_selected_station()

                   # Get the data
                   data_all <- data_measurements()

                   # Check if everything is available for the selection
                   shiny::validate(need(!is.null(start_time) &
                                          !is.null(end_time) &
                                          !is.null(selected_parameter) &
                                          !is.null(data_all) &
                                          !purrr::is_empty(selected_stations),
                                        "Not yet data selected" ) )

                   # Get the info for each selected station
                   station_info <- data_stations() %>%
                     dplyr::filter(selected == T)

                   # Filter the measurements
                   measurements_filt_stns <- data_all %>%
                     dplyr::filter(date > start_time & date < end_time &
                                     station %in% selected_stations &
                                     parameter == selected_parameter &
                                     value < selected_cutoff())

                   # Combine station_info with the measurements and keep relevant
                   # columns
                   measurements_filt_stns <-
                     dplyr::left_join(measurements_filt_stns,
                                      station_info, by = "station") %>%
                     dplyr::select(station, date, parameter, value, sd, label,
                                   group_name, col, size, station_type, linetype) %>%
                     # Keep for this dataset the label the same as the station. No changes for grouping yet.
                     dplyr::mutate(label = station)

                   # log_trace("mod com: number of selected stations {length(selected_stations)}")
                   # log_trace("mod com: names of selected stations {paste(selected_stations, sep = ' ', collapse = ' ')}")
                   log_trace("mod com: filtered measurements {nrow(measurements_filt_stns)}")
                   return(measurements_filt_stns)
                 })

                 # Calculate group mean ----
                 # Reactive to calculate the mean for each group
                 calc_group_mean <- reactive({
                   # Get the measurements of those stations
                   measurements <- filter_data_measurements()

                   # check if stations are selected
                   shiny::validate(need(!is_empty(measurements) &
                                          !dim(measurements)[1] == 0,
                                        "No data_stations"))

                   # Calculate group mean and sd
                   data_mean <- measurements %>%
                     # Set label to groupname
                     dplyr::mutate(label = dplyr::case_when(station_type ==
                                                              "sensor" ~ group_name,
                                                   T ~ station)) %>%
                     # Keep also the parameters for the plotting
                     dplyr::group_by(group_name, date, parameter, label, col,
                                     size, station_type, linetype) %>%
                     dplyr::summarise(value = mean(value, na.rm = T),
                                      number = n(),
                                      sd = mean(sd, na.rm = T)/sqrt(n())) %>%
                     dplyr::ungroup()

                   # Set sd of a sensor to a minimal value, different for pm10 and pm25
                   data_mean <- data_mean %>%
                     # Check for minimal sd for sensors
                     dplyr::mutate(
                       sd = dplyr::case_when(station_type == "sensor" &
                                               grepl("pm25", parameter, fixed = T) &
                                               sd < uc_min_pm25 ~ uc_min_pm25,
                                             station_type == "sensor" &
                                               grepl("pm10", parameter, fixed = T) &
                                               sd < uc_min_pm10 ~ uc_min_pm10,
                                             T ~ sd))

                   return(data_mean)

                 })

                 # Get knmi measurements ----
                 # the knmi measurements are excluded
                 # by the selected parameter in the measurements_filt_stns
                 get_knmi_measurements <- reactive({
                   # Get the start and end time to filter on
                   start_time <- selected_start_date()
                   end_time <- selected_end_date()

                   # Get selected stations
                   all_selected_stations <- get_selected_station()
                   selected_stations <- all_selected_stations[grep("KNMI", all_selected_stations)]

                   # Get the data
                   data_all <- data_measurements()

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

                 # To generate a message for the user
                 generate_message <- reactive({
                   # Check the number of sensors
                   sensor_count <- data_stations() %>%
                     dplyr::filter(selected == T &
                                     station_type == "sensor") %>%
                     count(station)

                    # give message if none are selected
                    if(purrr::is_empty(sensor_count$n)){
                     return("Je hebt nu niets geselecteerd.")
                   }
                   # give empty message nothing to be shown needed
                   return(" ")
                 })

                 # Return ----
                 return(list(
                   selected_measurements = reactive({filter_data_measurements()}),
                   knmi_measurements = reactive({get_knmi_measurements()}),
                   grouped_measurements = reactive({calc_group_mean()}),
                   message_selected = reactive({generate_message()})

                 ))

               })
}
