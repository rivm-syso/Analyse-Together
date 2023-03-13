###############################################
### Get data button ###
###############################################

# This is a module which get the data from the database
######################################################################
# Output Module
######################################################################

get_data_button_output <- function(id) {

  ns <- NS(id)
  tagList(
  actionButton(ns("get_data_button"), i18n$t("btn_get_data"), style="background-color: #ffe9b7")
  )
}

######################################################################
# Server Module
######################################################################

get_data_button_server <- function(id,
                                   data_measurements,
                                   data_stations,
                                   message_data,
                                   proj_or_mun,
                                   name_munproj,
                                   selected_start_date,
                                   selected_end_date,
                                   pool,
                                   measurements_con,
                                   stations_con,
                                   # Options for the colors
                                   col_cat,
                                   col_default,
                                   col_overload,
                                   # Options for the linetype
                                   line_cat,
                                   line_default,
                                   line_overload,
                                   # DEfault group name
                                   group_name_none
                                  ) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Observe if the get_data_button is clicked ----
    observeEvent(input$get_data_button, {
      # Get the selected choice
      type_choice <- proj_or_mun()
      # Name of municipality/project
      name_choice <- name_munproj()

      # Check if there is selected project/municipality
      shiny::validate(
        need(!is_empty((type_choice)),"Please, select gemeente of project"),
        need(!is_empty((name_choice)),"Please, select gemeente of project")
      )

      # Get the selected time period
      start_time <- selected_start_date()
      end_time <- selected_end_date()

      # Load the data from the caching database
      # Get the station names in the selected Municipality/project
      stations_name <- get_stations_from_selection(name_choice, type_choice, conn = pool)

      log_trace("get mod: {lubridate::now()} Get data from caching database ... ")

      # Get the data measurements of the selected Municipality/project in the period
      data_measurements$data_all <- get_measurements(measurements_con, stations_name, start_time, end_time)

      # Check if there is data in de caching, otherwise stop and give message
      if(nrow(data_measurements$data_all) == 0){
        message_data$data_in_dbs <- c(paste0("No data in ", type_choice, " ", name_choice))
        shiny::validate(need(F,message = paste0("No data in ", type_choice, " ", name_choice)))
      }
      message_data$data_in_dbs <- c(paste0("Data available in ", type_choice, " ", name_choice))

      # Create a pm10_kal and pm25_kl for reference stations
      data_measurements$data_all <- add_ref_kal(data_measurements$data_all)

      # Add uncertainty to the measurements of the sensors
      data_measurements$data_all <- add_uncertainty_sensor(data_measurements$data_all)

      # Get the information from the stations
      data_stations$data <- get_locations(stations_con, stations_name)

      # Take for each sensor 1 location and add the plot-colours etc.
      data_stations$data <- data_stations$data %>%
        dplyr::distinct(station, .keep_all = T) %>%
        # Add some specific info for the tool
        dplyr::mutate(selected = F, col = col_default, linetype = line_default,
                      station_type = "sensor", group_name = group_name_none,
                      label = station) %>%
        dplyr::mutate(station_type = ifelse(grepl("KNMI", station) == T, "KNMI",
                                            ifelse(grepl("NL", station) == T, "ref",
                                                   station_type))) %>%
        dplyr::mutate(linetype = ifelse(station_type == "ref", line_overload, linetype),
                      size = ifelse(station_type == "ref", 2,1))

      log_trace("get mod: {lubridate::now()} Data available in tool. ")

    })

    return(list(
      data_measurements = reactive({data_measurements$data_all}),
      data_stations = reactive({data_stations$data}),
      message_data = reactive({message_data$data_in_dbs})
    ))

  })
}
