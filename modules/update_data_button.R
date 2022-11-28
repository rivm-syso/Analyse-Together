update_data_button_output <- function(id) {

  ns <- NS(id)
  tagList(
  actionButton(ns("update_data_button"), "Update data", style="background-color: #ffe9b7")
  )
}


update_data_button_server <- function(id,
                                      select_mun_or_proj,
                                      choose_mun_or_proj,
                                      pool,
                                      measurements_con,
                                      stations_con
                                      ) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    btn <- eventReactive(input$update_data_button, {T})

    observeEvent(input$update_data_button, {

        # Check if there is user input ----
        # Get the selected choice
        type_choice <- select_mun_or_proj()
        # Name of municipality/porject
        name_choice <- choose_mun_or_proj()

        # Check if there is selected project/municipality
        shiny::validate(
          need(!is_empty((type_choice)),"Please, select gemeente of project"),
          need(!is_empty((name_choice)),"Please, select gemeente of project")
        )

        # Get the selected time period
        start_time <- selected_time$selected_start_date()
        end_time <- selected_time$selected_end_date()

        # TODO check of dit nodig is
        # Check if a time is selected, otherwise total time
        # if(is.null(start_time)|is.null(end_time)){
        #   start_time <- default_time$start_time
        #   end_time <- default_time$end_time
        # }

        # Load the data from the caching database ----
        # Get the station names in the selected Municipality/project
        stations_name <- get_stations_from_selection(name_choice, type_choice, conn = pool)

        log_trace("mod checkdata: {lubridate::now()} Get data from caching database ... ")

        # Get the data measurements of the selected Municipality/project
        data_measurements <- measurements_con %>% as.data.frame() %>%
          dplyr::mutate(date = lubridate::as_datetime(timestamp, tz = "Europe/Amsterdam")) %>%
          dplyr::filter(station %in% stations_name &
                          date > start_time &
                          date < end_time)

        # Create a pm10_kal and pm25_kl for reference stations
        cols_pivot <- data_measurements %>% dplyr::pull(parameter) %>% unique()
        data_measurements <- data_measurements %>%
          tidyr::pivot_wider(names_from = parameter, values_from = value) %>%
          dplyr::mutate(pm10_kal = dplyr::case_when(grepl("NL", station) ~ pm10,
                                                    T ~ pm10_kal),
                        pm25_kal = dplyr::case_when(grepl("NL", station) ~ pm25,
                                                    T ~ pm25_kal)) %>%
          tidyr::pivot_longer(cols = cols_pivot, names_to = "parameter", values_to = "value") %>%
          dplyr::group_by(station, date, parameter) %>%
          dplyr::slice(which.max(!is.na(value))) %>%
          dplyr::ungroup()

        # Add uncertainty to the measurements of the sensors
        data_measurements <- data_measurements %>%
          dplyr::mutate(sd = dplyr::case_when(parameter == "pm25_kal" & !grepl("NL", station) ~ 5.3,
                                              parameter == "pm25" & !grepl("NL", station) ~ 5.3,
                                              parameter == "pm10_kal" & !grepl("NL", station) ~ 8.5,
                                              parameter == "pm10" & !grepl("NL", station) ~ 8.5,
                                              T ~ 0))

        # Get the information from the sensors
        data_sensors <- stations_con %>% as.data.frame() %>%
          dplyr::distinct(station, .keep_all = T) %>%
          dplyr::filter(station %in% stations_name) %>%
          dplyr::mutate(selected = F, col = col_default, linetype = line_default, station_type = "sensor") %>%
          dplyr::mutate(station_type = ifelse(grepl("KNMI", station) == T, "KNMI", ifelse(grepl("NL", station) == T, "LML", station_type))) %>%
          dplyr::mutate(linetype = ifelse(station_type == "LML", line_overload, linetype),
                        size = ifelse(station_type == "LML", 2,1))

        log_trace("mod checkdata: {lubridate::now()} Data available in tool. ")

        return(list(data_measurements = data_measurements, data_sensors = data_sensors))
      })

  })
}
