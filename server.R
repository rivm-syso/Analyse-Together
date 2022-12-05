
# Define server logic                                                       ====
shinyServer(function(global, input, output, session) {

  # To change the language in the tool
  observeEvent(input$selected_language, {
    # Here is where we update language in session
    shiny.i18n::update_lang(session, input$selected_language)
  })

  # ReactiveValues to store the data
  data_measurements <- reactiveValues()
  data_stations <- reactiveValues()

  # The pickerInput for component selection
  select_component <- component_selection_server("select_component",
                                                 comp_choices)

  # select project/mun
  proj_or_mun_select <- project_or_mun_selection_server("proj_or_mun_select",
                                                        select_choices)

  # The dateRangeInput for date range selection
  select_date_range <- date_range_server("select_date_range",
                                         list(start_time = default_time$start_time, end_time = default_time$end_time))
                                         #communication_stuff$selected_time())

  # choose proj/mun
  choice_select <- choice_selection_server("choice_select",
                                           proj_or_mun_select = proj_or_mun_select,
                                           mun_choices = mun_choices,
                                           proj_choices = proj_choices)

  # Get metadata
  #meta_table <- metadata_server("meta_table", communication_stuff)

  # The Map
  #map <- show_map_server("map", communication_stuff, update_data = update_data_button)

  # The bar plot
  #barplot <- barplot_server("barplot_plot", communication_stuff, overview_component, theme_plots)

  # The timeseries plot
  #timeseries_plot <- timeseries_server(id = "timeseries_plot", com_module = communication_stuff, overview_component, theme_plots)

  # The pollutionrose plot
  #pollrose_plot <- pollrose_server("pollrose_plot", communication_stuff)

  # The timevariation plot
  #timevar_plot_weekly <- timevar_weekly_server("timevar_plot_weekly", communication_stuff)
  #timevar_plot_daily <- timevar_daily_server("timevar_plot_daily", communication_stuff)

  # The communication module
  # communication_stuff <- communication_server("test_comm_output",
  #                                             update_data = update_data_button,
  #                                             download_data_123 = download_api_button,
  #                                             pool = pool,
  #                                             measurements_con = measurements_con,
  #                                             stations_con = stations_con,
  #                                             meta, # TODO willen we hier wat mee?
  #                                             selected_parameter = select_component,
  #                                             selected_time = select_date_range,
  #                                             default_time = default_time,
  #                                             select_mun_or_proj = proj_or_mun_select,
  #                                             choose_mun_or_proj = choice_select,
  #                                             # TODO Get the selected stations form the map
  #                                             #selected_stations = c("SSK_LH004"),
  #                                             selected_stations = map,
  #                                             # Options for the colors
  #                                             col_cat,
  #                                             col_default,
  #                                             col_overload,
  #                                             # Options for the linetype
  #                                             line_cat,
  #                                             line_default,
  #                                             line_overload
  #                                             )

  # download_api_button <- download_api_button_server("dl_btn_pushed", proj_or_mun_select , choice_select, select_date_range, pool, que)
  # get_data_button <- get_data_button_server("get_data",
                                                  # select_mun_or_proj = proj_or_mun_select,
                                                  # choose_mun_or_proj = choice_select,
                                                  # select_time = select_date_range,
                                                  # pool = pool,
                                                  # measurements_con = measurements_con,
                                                  # stations_con = stations_con)
  show_availability_server("show_availability", data_measurements)

  observeEvent(input$get_data_button, {
    # Check if there is user input ----
    # Get the selected choice
    type_choice <- proj_or_mun_select()
    # Name of municipality/porject
    name_choice <- choice_select()

    # Check if there is selected project/municipality
    shiny::validate(
      need(!is_empty((type_choice)),"Please, select gemeente of project"),
      need(!is_empty((name_choice)),"Please, select gemeente of project")
    )

    # Get the selected time period
    start_time <- select_date_range$selected_start_date()
    end_time <- select_date_range$selected_end_date()

    # Load the data from the caching database ----
    # Get the station names in the selected Municipality/project
    stations_name <- get_stations_from_selection(name_choice, type_choice, conn = pool)

    log_trace("{lubridate::now()} Get data from caching database ... ")

      # Get the data measurements of the selected Municipality/project in the period
      data_measurements$data <- get_measurements(measurements_con, stations_name, start_time, end_time)
      shiny::validate(need(nrow(data_measurements$data) > 0, "No data in municipality"))

      # Create a pm10_kal and pm25_kl for reference stations
      data_measurements$data <- add_ref_kal(data_measurements$data)

      # Add uncertainty to the measurements of the sensors
      data_measurements$data <- add_uncertainty_sensor(data_measurements$data)

      # Get the information from the stations
      data_stations$data <- get_locations(stations_con, stations_name)

      # Take for each sensor 1 location
      data_stations$data <- data_stations$data %>%
        dplyr::distinct(station, .keep_all = T) %>%
        # Add some specific info for the tool
        dplyr::mutate(selected = F, col = col_default, linetype = line_default, station_type = "sensor") %>%
        dplyr::mutate(station_type = ifelse(grepl("KNMI", station) == T, "KNMI", ifelse(grepl("NL", station) == T, "LML", station_type))) %>%
        dplyr::mutate(linetype = ifelse(station_type == "LML", line_overload, linetype),
                      size = ifelse(station_type == "LML", 2,1))

    log_trace("{lubridate::now()} Data available in tool. ")

  })



  #view_que_server("view_que", que)

})
