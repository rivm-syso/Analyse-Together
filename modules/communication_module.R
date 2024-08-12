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

                   data_stations_fun <- data_stations()
                   measurements_filt_stns <- filter_data_measurements_fun(
                     start_time,
                     end_time,
                     100,
                     selected_parameter,
                     data_stations_fun,
                     data_all
                   )

                   # log_trace("mod com: number of selected stations {length(selected_stations)}")
                   # log_trace("mod com: names of selected stations {paste(selected_stations, sep = ' ', collapse = ' ')}")
                   log_trace("mod com: filtered measurements {nrow(measurements_filt_stns)}")
                   return(measurements_filt_stns)
                 })

                 # Calculate group mean ----
                 # Reactive to calculate the mean for each group
                 calc_group_mean <- reactive({
                   data_mean <- calc_group_mean_fun(filter_data_measurements(),
                                                    uc_min_pm10,
                                                    uc_min_pm25)

                   return(data_mean)

                 })

                 # Get knmi measurements ----
                 # the knmi measurements are excluded
                 # by the selected parameter in the measurements_filt_stns
                 get_knmi_measurements <- reactive({
                   # Get the start and end time to filter on
                   start_time <- selected_start_date()
                   end_time <- selected_end_date()

                   measurements_filt_knmi <- get_knmi_measurements_fun(start_time,
                                                                       end_time,
                                                                       data_measurements(),
                                                                       data_stations())
#

                   # log_trace("mod com: number of selected stations {length(selected_stations)}")
                   # log_trace("mod com: names of selected stations {paste(selected_stations, sep = ' ', collapse = ' ')}")
                   log_trace("mod com: filtered measurements KNMI {nrow(measurements_filt_knmi)}")

                   return(measurements_filt_knmi)
                 })

                 # Calculate the cutoff value depending on the sensor data ----
                 # distribution
                 calc_cut_off <- reactive({
                   # take all the sensor measurements
                   names_snsrs <- data_stations() %>%
                     dplyr::filter(station_type == "sensor" )

                   # take all the sensor measurements
                   data_snsrs <- data_measurements() %>%
                     dplyr::filter(station %in% names_snsrs$station)

                   # Calculate the cut-off value
                   # take the upper 10%percentile
                   cut_off_value <- quantile(data_snsrs$value, .9, na.rm = T)

                   return(cut_off_value)
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
                   cut_off_value = reactive({calc_cut_off()}),
                   message_selected = reactive({generate_message()})

                 ))

               })
}
